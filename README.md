# Classical Ciphers

1. Download and install Docker: https://www.docker.com/community-edition#/download
2. Start Docker (steps 3-6 won't work unless the Docker daemon is running).
3. Run 'setup-env.sh' script; this will build a linux image with the necessary libraries and programs to be able to compile the source code.
4. Run 'launch-env.sh' script to boot into a terminal prompt for the linux image.
5. Run 'compile.sh', this will build a binary from the 'Main.re' source file.
6. Run 'run.sh <CIPHER NAME> <KEY> <MODE> <INPUT FILE> <OUTPUT FILE>'.