use rsvm::objectmemory::{
    ImageFormat,
    dist_format::DistFormat,
    text_format::TextFormat
};

fn main() -> Result<(), Box<std::error::Error>> {
    let args = std::env::args().collect::<Vec<_>>();
    let memory = DistFormat::load(&args[1])?;
    TextFormat::save(&args[2], &memory)?;
    Ok(())
}