pub mod interpreter;
pub mod objectmemory;

fn main() -> std::io::Result<()> {
    use objectmemory::ImageFormat;
    let memory = objectmemory::dist_format::DistFormat::load("vm.image")?;
    let mut interpreter = interpreter::Interpreter::boot(memory);
    interpreter.interpret();

    Ok(())
}
