import sys
import os
import subprocess

def validate_command(command):
    # Example validation: Ensure the command does not contain potentially dangerous substrings
    forbidden_substrings = ['rm -rf', 'sudo', 'dd if=', 'curl', 'wget']
    for substring in forbidden_substrings:
        if substring in command:
            return False, f"Error: Command contains forbidden substring '{substring}'"
        
    # Check if the command starts with 'copernicusmarine'
    if not command.startswith('copernicusmarine'):
        return False, "Error: Command must start with 'copernicusmarine'"

    # Remove 'copernicusmarine' from the start
    command = command[len('copernicusmarine'):].strip()

    # Check for specific commands and their arguments
    if command.startswith('subset'):
        # Check for required arguments for 'subset' command
        if not ('--dataset-id' in command or '--dataset-url' in command):
            return False, "Error: 'subset' command must contain either '--dataset-id' or '--dataset-url'"
    elif command.startswith('get'):
        # Check for required arguments for 'get' command
        if not ('--dataset-id' in command or '--dataset-url' in command):
            return False, "Error: 'get' command must contain either '--dataset-id' or '--dataset-url'"
    elif command.startswith('login') or command.startswith('describe'):
        return False, "Sorry, this tool accepts only the 'subset' and 'get' commands of the Copernicus Marine Toolbox"
    else:
        return False, "Error: Command must be 'subset' or 'get'"

    
    return True, None

def main():
    # Check if a filename argument is provided
    if len(sys.argv) != 2:
        print("Usage: python check.py <config_file>")
        sys.exit(1)

    # Get the filename from the command line argument
    config_file = sys.argv[1]

    # Check if the file exists
    if not os.path.isfile(config_file):
        print(f"Error: File '{config_file}' does not exist.")
        sys.exit(1)

    # Read the content of the file
    with open(config_file, 'r') as file:
        command = file.read().strip()

    # Validate the command
    is_valid, error_message = validate_command(command)
    if not is_valid:
        print(error_message)
        sys.exit(1)

    # Execute the command
    try:
        subprocess.run(command, shell=True, check=True)
    except subprocess.CalledProcessError as e:
        print(f"Error: Command failed with exit code {e.returncode}")
        sys.exit(1)

if __name__ == "__main__":
    main()
