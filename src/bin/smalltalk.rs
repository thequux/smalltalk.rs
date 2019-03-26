use rsvm::interpreter;
use rsvm::objectmemory;

fn main() -> std::io::Result<()> {
    use objectmemory::ImageFormat;
    println!("Loading");
    let memory = objectmemory::dist_format::DistFormat::load("vm.image")?;
    println!("Booting");
    let mut interpreter = interpreter::Interpreter::boot(memory);
    println!("Starting execution");
    interpreter.interpret();

    Ok(())
}
