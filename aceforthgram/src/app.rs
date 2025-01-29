use std::env;
use std::path::Path;

use aceforthlib::Runner;
use tokio::fs;
use tokio::io::AsyncWriteExt;

fn filename(user_id: u64) -> String {
    let folder = env::var("SNAPSHOTS_PATH").unwrap_or(".".to_string());
    let path = Path::new(&folder);
    let full_path =path.join(format!("{}.sav", user_id));
    full_path.to_str().unwrap().to_string()
}

fn filename_log(user_id: u64) -> String {
    let folder = env::var("LOGS_PATH").unwrap_or(".".to_string());
    let path = Path::new(&folder);
    let full_path =path.join(format!("{}.log", user_id));
    full_path.to_str().unwrap().to_string()
}

pub async fn build_runner(user_id: u64) -> Runner {
    let mut runner = Runner::new(false, false, false);

    let content = fs::read(filename(user_id)).await;

    let snapshot = match content {
        Ok(data) => data,
        Err(_) => {
            runner.prepare();
            return runner;
        }
    };

    if runner.prepare_with_snapshot(&snapshot).is_err() {
        runner.prepare();
    }
    runner
}

pub async fn persist_runner(user_id: u64, runner: &Runner) {
    let snapshot = runner.save_snapshot();
    let result = fs::write(filename(user_id), snapshot).await;
    if let Err(e) = result {
        eprintln!("Error saving snapshot: {}", e);
    }
}

pub fn execute(runner: &mut Runner, commands: &str) -> Vec<String> {
    let mut output = Vec::new();
    for line in commands.lines() {
        let response = runner.execute_command(line, 1_000_000_000);
        output.push(response.output);

        if let Some(error_code) = response.error_code {
            output.push(format!(
                ">>> Error code {}: {}",
                error_code,
                Runner::error_message(error_code)
            ));
        }

        if let Some(pending_input) = response.pending_input {
            output.push(format!("? {}", pending_input));
            return output;
        }
        if response.error_code.is_some() {
            return output;
        }
    }
    output
}

pub fn screen_command(runner: &Runner) -> Vec<u8> {
    runner.save_screen_image(true /* scanlines */)
}

pub fn vis_command(runner: &mut Runner) -> bool {
    runner.toggle_invis()
}

pub async fn reset_command(user_id: u64) {
    let _ = fs::remove_file(filename(user_id)).await;
}

pub async fn log_command(name: &str, user_id: u64, text: &str) {
    let file = fs::OpenOptions::new()
        .write(true)
        .append(true)
        .open(filename_log(user_id))
        .await;

    match file {
        Ok(mut file) => {
            let log = format!("{}\n", text);
            let _ = file.write_all(log.as_bytes()).await;
        }
        Err(_) => {
            let text = format!("{}\n{}\n", name, text);
            let _ = fs::write(filename_log(user_id), text).await;
            return;
        }
    }
}
