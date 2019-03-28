use rsvm::interpreter;
use rsvm::objectmemory;

fn main() -> Result<(), failure::Error> {
    use ::std::env::args;
    let args = args().collect::<Vec<_>>();
    use objectmemory::ImageFormat;
    println!("Loading");
//    let memory = objectmemory::dist_format::DistFormat::load("vm.image")?;
    let memory = objectmemory::text_format::TextFormat::load(&args[1])?;
    println!("Booting");
    let mut interpreter = interpreter::Interpreter::boot(memory);
    println!("Starting execution");
    interpreter.interpret();

    Ok(())
}
