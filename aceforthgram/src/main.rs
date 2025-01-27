use aceforthlib::GRAPH_CHARS;
use teloxide::{dispatching::UpdateHandler, prelude::*, types::User, types::InputFile, utils::command::BotCommands};

mod app;

type HandlerResult = Result<(), Box<dyn std::error::Error + Send + Sync>>;

#[tokio::main]
async fn main() {
    pretty_env_logger::init();
    log::info!("Starting throw dice bot...");

    let bot = Bot::from_env();

    Dispatcher::builder(bot, schema())
        .enable_ctrlc_handler()
        .build()
        .dispatch()
        .await;
    }

#[derive(BotCommands, Clone)]
#[command(rename_rule = "lowercase")]
enum Command {
    /// Display this text.
    Help,
    /// Show a screenshot.
    Screen,
    /// Show the graphical characters for easy copy-pasting.
    Graphs,
    /// Toggles invisible mode.
    Vis,
    /// Reset the machine.
    Reset,
}

fn schema() -> UpdateHandler<Box<dyn std::error::Error + Send + Sync + 'static>> {
    use dptree::case;

    let command_handler = teloxide::filter_command::<Command, _>()
        .branch(case![Command::Help].endpoint(help_command))
        .branch(case![Command::Screen].endpoint(screen_command))
        .branch(case![Command::Graphs].endpoint(graphs_command))
        .branch(case![Command::Vis].endpoint(vis_command))
        .branch(case![Command::Reset].endpoint(reset_command));

    let message_handler = Update::filter_message()
        .filter_map(|update: Update| update.from().cloned())
        .branch(command_handler)
        .branch(Message::filter_text().endpoint(handle_message));

    message_handler
}

async fn handle_message(bot: Bot, user: User, text: String) -> HandlerResult {
    let mut runner = app::build_runner(user.id.0).await;
    let answer = app::execute(&mut runner, &text);
    app::persist_runner(user.id.0, &runner).await;

    if !answer.is_empty() {
        bot.send_message(user.id, answer.join("\n")).await?;
    }
    Ok(())
}

async fn help_command(bot: Bot, user: User) -> HandlerResult {
    bot.send_message(user.id, Command::descriptions().to_string()).await?;
    Ok(())
}

async fn screen_command(bot: Bot, user: User /*, msg: Message*/) -> HandlerResult {
    let runner = app::build_runner(user.id.0).await;
    let png = app::screen_command(&runner);
    let photo = InputFile::memory(png);
    bot.send_photo(user.id, photo).await?;
    Ok(())
}

async fn graphs_command(bot: Bot, user: User) -> HandlerResult {
    bot.send_message(user.id, format!("The Jupiter ACE semigraphic characters are: {}", GRAPH_CHARS)).await?;
    Ok(())
}

async fn vis_command(bot: Bot, user: User) -> HandlerResult {
    let mut runner = app::build_runner(user.id.0).await;
    let vis = app::vis_command(&mut runner);
    app::persist_runner(user.id.0, &runner).await;
    bot.send_message(user.id, format!("Invisible mode is now {}", if vis { "active" } else { "disabled" } )).await?;
    Ok(())
}

async fn reset_command(bot: Bot, user: User) -> HandlerResult {
    app::reset_command(user.id.0).await;
    bot.send_message(user.id, "Engine restarted").await?;
    Ok(())
}
