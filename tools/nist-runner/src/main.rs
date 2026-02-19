use clap::Parser;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

/// NIST CCVS Test Runner for cobolc
#[derive(Parser, Debug)]
#[command(name = "nist-runner", version, about)]
struct Cli {
    /// Path to the directory containing COBOL test files (.cob)
    #[arg(short, long)]
    suite_dir: PathBuf,

    /// Path to the cobolc binary
    #[arg(short, long, default_value = "cobolc")]
    compiler: PathBuf,

    /// Test module to run (NC, SM, IC, SQ, RL, IX, ST, IF, RW, SG, or "all")
    #[arg(short, long, default_value = "all")]
    module: String,

    /// Path to known_failures.toml
    #[arg(short, long)]
    known_failures: Option<PathBuf>,

    /// Output format: text, json
    #[arg(long, default_value = "text", value_parser = ["text", "json"])]
    format: String,

    /// Timeout in seconds for each test program execution
    #[arg(long, default_value = "30")]
    timeout: u64,

    /// Only run tests matching this prefix (e.g., "NC101")
    #[arg(long)]
    filter: Option<String>,

    /// Show verbose output (compilation errors, stdout, etc.)
    #[arg(short, long)]
    verbose: bool,
}

/// Known failures configuration loaded from TOML.
#[derive(Debug, Deserialize, Default)]
struct KnownFailures {
    /// Tests that are expected to fail compilation.
    #[serde(default)]
    compile_fail: Vec<String>,

    /// Tests that are expected to fail at runtime (wrong output or crash).
    #[serde(default)]
    runtime_fail: Vec<String>,

    /// Tests to skip entirely.
    #[serde(default)]
    skip: Vec<String>,

    /// Optional reason annotations (test_name -> reason).
    #[serde(default)]
    reasons: BTreeMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
enum TestOutcome {
    Pass,
    Fail,
    ExpectedFail,
    UnexpectedPass,
    Skip,
    CompileFail,
    Timeout,
}

impl std::fmt::Display for TestOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestOutcome::Pass => write!(f, "PASS"),
            TestOutcome::Fail => write!(f, "FAIL"),
            TestOutcome::ExpectedFail => write!(f, "EXPECTED_FAIL"),
            TestOutcome::UnexpectedPass => write!(f, "UNEXPECTED_PASS"),
            TestOutcome::Skip => write!(f, "SKIP"),
            TestOutcome::CompileFail => write!(f, "COMPILE_FAIL"),
            TestOutcome::Timeout => write!(f, "TIMEOUT"),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
struct TestResult {
    name: String,
    outcome: TestOutcome,
    /// Number of individual PASS assertions in the test output.
    pass_count: u32,
    /// Number of individual FAIL assertions in the test output.
    fail_count: u32,
    /// Wall-clock duration.
    duration_ms: u64,
    /// Optional detail message (compiler error, diff, etc.)
    detail: Option<String>,
}

#[derive(Debug, Serialize)]
struct Summary {
    total: usize,
    pass: usize,
    fail: usize,
    expected_fail: usize,
    unexpected_pass: usize,
    skip: usize,
    compile_fail: usize,
    timeout: usize,
    regressions: usize,
    results: Vec<TestResult>,
}

fn main() {
    let cli = Cli::parse();

    // Load known failures
    let known_failures = match &cli.known_failures {
        Some(path) => {
            let content = fs::read_to_string(path).unwrap_or_else(|e| {
                eprintln!("Warning: Could not read known_failures file {:?}: {}", path, e);
                String::new()
            });
            if content.is_empty() {
                KnownFailures::default()
            } else {
                toml::from_str(&content).unwrap_or_else(|e| {
                    eprintln!("Warning: Could not parse known_failures TOML: {}", e);
                    KnownFailures::default()
                })
            }
        }
        None => KnownFailures::default(),
    };

    // Discover test files
    let test_files = discover_tests(&cli.suite_dir, &cli.module, cli.filter.as_deref());

    if test_files.is_empty() {
        eprintln!("No test files found in {:?} for module '{}'", cli.suite_dir, cli.module);
        std::process::exit(1);
    }

    if cli.format == "text" {
        println!("NIST CCVS Test Runner");
        println!("====================");
        println!(
            "Suite: {}  Module: {}  Tests: {}",
            cli.suite_dir.display(),
            cli.module,
            test_files.len()
        );
        println!("Compiler: {}", cli.compiler.display());
        println!();
    }

    let mut results = Vec::new();

    for test_path in &test_files {
        let test_name = test_path
            .file_stem()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();

        // Check if we should skip
        if known_failures.skip.contains(&test_name) {
            let result = TestResult {
                name: test_name.clone(),
                outcome: TestOutcome::Skip,
                pass_count: 0,
                fail_count: 0,
                duration_ms: 0,
                detail: known_failures.reasons.get(&test_name).cloned(),
            };
            if cli.format == "text" {
                print_result(&result);
            }
            results.push(result);
            continue;
        }

        let is_expected_compile_fail = known_failures.compile_fail.contains(&test_name);
        let is_expected_runtime_fail = known_failures.runtime_fail.contains(&test_name);

        let result = run_test(
            &cli.compiler,
            test_path,
            &test_name,
            is_expected_compile_fail,
            is_expected_runtime_fail,
            Duration::from_secs(cli.timeout),
            cli.verbose,
            known_failures.reasons.get(&test_name).cloned(),
        );

        if cli.format == "text" {
            print_result(&result);
        }

        results.push(result);
    }

    // Build summary
    let summary = build_summary(results);

    match cli.format.as_str() {
        "json" => {
            let json = serde_json::to_string_pretty(&summary).expect("Failed to serialize JSON");
            println!("{}", json);
        }
        _ => {
            print_summary(&summary);
        }
    }

    // Exit code: 0 if no regressions, 1 otherwise
    if summary.regressions > 0 {
        std::process::exit(1);
    }
}

/// Discover .cob files in the suite directory, filtered by module prefix.
fn discover_tests(suite_dir: &Path, module: &str, filter: Option<&str>) -> Vec<PathBuf> {
    let mut tests: Vec<PathBuf> = Vec::new();

    let entries = match fs::read_dir(suite_dir) {
        Ok(entries) => entries,
        Err(e) => {
            eprintln!("Error reading directory {:?}: {}", suite_dir, e);
            return tests;
        }
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().map_or(false, |ext| ext == "cob" || ext == "CBL" || ext == "cbl") {
            let name = path
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();

            // Filter by module prefix (first 2 characters, e.g., "NC", "SM")
            if module != "all" {
                let prefix = if name.len() >= 2 {
                    &name[..2]
                } else {
                    &name
                };
                if !prefix.eq_ignore_ascii_case(&module) {
                    continue;
                }
            }

            // Filter by user-provided prefix
            if let Some(f) = filter {
                if !name.starts_with(f) {
                    continue;
                }
            }

            tests.push(path);
        }
    }

    tests.sort();
    tests
}

/// Run a single NIST test: compile, execute, parse output.
fn run_test(
    compiler: &Path,
    source: &Path,
    test_name: &str,
    is_expected_compile_fail: bool,
    is_expected_runtime_fail: bool,
    timeout: Duration,
    verbose: bool,
    reason: Option<String>,
) -> TestResult {
    let start = Instant::now();

    // Create a temp directory for this test
    let tmp_dir = std::env::temp_dir().join(format!("nist-{}", test_name));
    let _ = fs::create_dir_all(&tmp_dir);
    let binary = tmp_dir.join(test_name);

    // Step 1: Compile
    let compile_result = Command::new(compiler)
        .arg(source)
        .arg("-o")
        .arg(&binary)
        .output();

    let compile_output = match compile_result {
        Ok(output) => output,
        Err(e) => {
            let elapsed = start.elapsed();
            let _ = fs::remove_dir_all(&tmp_dir);
            let detail = format!("Failed to invoke compiler: {}", e);
            return TestResult {
                name: test_name.to_string(),
                outcome: if is_expected_compile_fail {
                    TestOutcome::ExpectedFail
                } else {
                    TestOutcome::CompileFail
                },
                pass_count: 0,
                fail_count: 0,
                duration_ms: elapsed.as_millis() as u64,
                detail: Some(detail),
            };
        }
    };

    if !compile_output.status.success() {
        let elapsed = start.elapsed();
        let stderr = String::from_utf8_lossy(&compile_output.stderr).to_string();
        if verbose {
            eprintln!("  [compile error] {}", stderr.trim());
        }
        let _ = fs::remove_dir_all(&tmp_dir);
        return TestResult {
            name: test_name.to_string(),
            outcome: if is_expected_compile_fail {
                TestOutcome::ExpectedFail
            } else {
                TestOutcome::CompileFail
            },
            pass_count: 0,
            fail_count: 0,
            duration_ms: elapsed.as_millis() as u64,
            detail: Some(stderr),
        };
    }

    // If we expected it to fail compilation but it succeeded, that's an unexpected pass
    // (but we still run it to see if it produces correct output)

    // Step 2: Execute
    let run_result = run_with_timeout(&binary, timeout);

    let elapsed = start.elapsed();
    let _ = fs::remove_dir_all(&tmp_dir);

    match run_result {
        RunResult::Success(stdout) => {
            let (pass_count, fail_count) = parse_nist_output(&stdout);

            if verbose {
                eprintln!("  [stdout] {}", stdout.trim());
            }

            let outcome = if fail_count > 0 {
                // Test had failures
                if is_expected_runtime_fail || is_expected_compile_fail {
                    TestOutcome::ExpectedFail
                } else {
                    TestOutcome::Fail
                }
            } else if pass_count > 0 {
                // Test had passes and no failures
                if is_expected_runtime_fail || is_expected_compile_fail {
                    TestOutcome::UnexpectedPass
                } else {
                    TestOutcome::Pass
                }
            } else {
                // No PASS/FAIL markers found - treat as failure
                if is_expected_runtime_fail || is_expected_compile_fail {
                    TestOutcome::ExpectedFail
                } else {
                    TestOutcome::Fail
                }
            };

            TestResult {
                name: test_name.to_string(),
                outcome,
                pass_count,
                fail_count,
                duration_ms: elapsed.as_millis() as u64,
                detail: reason.or_else(|| {
                    if fail_count > 0 {
                        Some(format!("{} pass, {} fail", pass_count, fail_count))
                    } else if pass_count == 0 {
                        Some("No PASS/FAIL markers in output".to_string())
                    } else {
                        None
                    }
                }),
            }
        }
        RunResult::RuntimeError(stderr) => {
            if verbose {
                eprintln!("  [runtime error] {}", stderr.trim());
            }
            TestResult {
                name: test_name.to_string(),
                outcome: if is_expected_runtime_fail || is_expected_compile_fail {
                    TestOutcome::ExpectedFail
                } else {
                    TestOutcome::Fail
                },
                pass_count: 0,
                fail_count: 0,
                duration_ms: elapsed.as_millis() as u64,
                detail: Some(stderr),
            }
        }
        RunResult::Timeout => TestResult {
            name: test_name.to_string(),
            outcome: if is_expected_runtime_fail || is_expected_compile_fail {
                TestOutcome::ExpectedFail
            } else {
                TestOutcome::Timeout
            },
            pass_count: 0,
            fail_count: 0,
            duration_ms: elapsed.as_millis() as u64,
            detail: Some(format!("Timed out after {}s", timeout.as_secs())),
        },
    }
}

enum RunResult {
    Success(String),
    RuntimeError(String),
    Timeout,
}

/// Run a binary with a timeout.
fn run_with_timeout(binary: &Path, timeout: Duration) -> RunResult {
    let mut child = match Command::new(binary)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(e) => return RunResult::RuntimeError(format!("Failed to execute: {}", e)),
    };

    let start = Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                let stdout = child
                    .stdout
                    .take()
                    .map(|mut s| {
                        let mut buf = String::new();
                        io::Read::read_to_string(&mut s, &mut buf).ok();
                        buf
                    })
                    .unwrap_or_default();
                let stderr = child
                    .stderr
                    .take()
                    .map(|mut s| {
                        let mut buf = String::new();
                        io::Read::read_to_string(&mut s, &mut buf).ok();
                        buf
                    })
                    .unwrap_or_default();

                if status.success() {
                    return RunResult::Success(stdout);
                } else {
                    let detail = if stderr.is_empty() {
                        format!("Process exited with status: {}", status)
                    } else {
                        stderr
                    };
                    return RunResult::RuntimeError(detail);
                }
            }
            Ok(None) => {
                // Still running
                if start.elapsed() > timeout {
                    let _ = child.kill();
                    return RunResult::Timeout;
                }
                std::thread::sleep(Duration::from_millis(50));
            }
            Err(e) => {
                return RunResult::RuntimeError(format!("Error waiting for process: {}", e));
            }
        }
    }
}

/// Parse NIST CCVS output for PASS/FAIL markers.
///
/// NIST tests typically output lines like:
///   TEST-NAME  PASS
///   TEST-NAME  FAIL
///
/// We count occurrences of "PASS" and "FAIL" as individual test case results.
fn parse_nist_output(stdout: &str) -> (u32, u32) {
    let mut pass_count: u32 = 0;
    let mut fail_count: u32 = 0;

    for line in stdout.lines() {
        let trimmed = line.trim();

        // Look for lines containing PASS or FAIL
        // NIST convention: lines with test results contain the word PASS or FAIL
        // We need to be careful not to match partial words, but NIST output is typically
        // very structured with PASS/FAIL as separate tokens.

        // Split into tokens and look for PASS/FAIL
        let tokens: Vec<&str> = trimmed.split_whitespace().collect();
        for token in &tokens {
            if *token == "PASS" || *token == "PASS." {
                pass_count += 1;
            } else if *token == "FAIL" || *token == "FAIL." {
                fail_count += 1;
            }
        }
    }

    (pass_count, fail_count)
}

fn print_result(result: &TestResult) {
    let color = match result.outcome {
        TestOutcome::Pass => "\x1b[32m",          // green
        TestOutcome::Fail => "\x1b[31m",           // red
        TestOutcome::ExpectedFail => "\x1b[33m",   // yellow
        TestOutcome::UnexpectedPass => "\x1b[35m", // magenta
        TestOutcome::Skip => "\x1b[36m",           // cyan
        TestOutcome::CompileFail => "\x1b[31m",    // red
        TestOutcome::Timeout => "\x1b[31m",        // red
    };
    let reset = "\x1b[0m";

    let detail_str = match &result.detail {
        Some(d) => {
            // Show first line of detail only
            let first_line = d.lines().next().unwrap_or("");
            if first_line.is_empty() {
                String::new()
            } else {
                format!(" ({})", first_line.trim())
            }
        }
        None => String::new(),
    };

    println!(
        "  {}{:16}{} {:12} {:>4}ms{}",
        color, result.outcome, reset, result.name, result.duration_ms, detail_str
    );
}

fn build_summary(results: Vec<TestResult>) -> Summary {
    let total = results.len();
    let pass = results.iter().filter(|r| r.outcome == TestOutcome::Pass).count();
    let fail = results
        .iter()
        .filter(|r| r.outcome == TestOutcome::Fail)
        .count();
    let expected_fail = results
        .iter()
        .filter(|r| r.outcome == TestOutcome::ExpectedFail)
        .count();
    let unexpected_pass = results
        .iter()
        .filter(|r| r.outcome == TestOutcome::UnexpectedPass)
        .count();
    let skip = results
        .iter()
        .filter(|r| r.outcome == TestOutcome::Skip)
        .count();
    let compile_fail = results
        .iter()
        .filter(|r| r.outcome == TestOutcome::CompileFail)
        .count();
    let timeout = results
        .iter()
        .filter(|r| r.outcome == TestOutcome::Timeout)
        .count();

    // Regressions = tests that FAIL or COMPILE_FAIL or TIMEOUT that are NOT in known_failures
    // i.e., fail + compile_fail + timeout counts already exclude expected failures
    let regressions = fail + compile_fail + timeout;

    Summary {
        total,
        pass,
        fail,
        expected_fail,
        unexpected_pass,
        skip,
        compile_fail,
        timeout,
        regressions,
        results,
    }
}

fn print_summary(summary: &Summary) {
    println!();
    println!("====================");
    println!("NIST CCVS Test Summary");
    println!("====================");
    println!("Total:            {}", summary.total);
    println!("\x1b[32mPass:             {}\x1b[0m", summary.pass);
    println!("\x1b[31mFail:             {}\x1b[0m", summary.fail);
    println!("\x1b[33mExpected Fail:    {}\x1b[0m", summary.expected_fail);
    println!(
        "\x1b[35mUnexpected Pass:  {}\x1b[0m",
        summary.unexpected_pass
    );
    println!("\x1b[36mSkip:             {}\x1b[0m", summary.skip);
    println!("\x1b[31mCompile Fail:     {}\x1b[0m", summary.compile_fail);
    println!("\x1b[31mTimeout:          {}\x1b[0m", summary.timeout);
    println!("--------------------");

    if summary.regressions > 0 {
        println!(
            "\x1b[31mRegressions:      {} (FAILING)\x1b[0m",
            summary.regressions
        );
    } else {
        println!("\x1b[32mRegressions:      0 (OK)\x1b[0m");
    }
}
