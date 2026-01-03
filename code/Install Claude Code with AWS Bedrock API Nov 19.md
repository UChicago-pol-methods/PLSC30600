# Install Claude Code with AWS Bedrock API

This guide outlines the steps to install Claude Code and configure it to use the AWS Bedrock API.

* [Claude on Amazon Bedrock](https://platform.claude.com/docs/en/build-with-claude/claude-on-amazon-bedrock)

## 1. Install Claude Code

Follow the quick start guide for Claude Code, or directly install it based on your operating system.

* [Claude Code Quick Start](https://docs.claude.com/en/docs/claude-code/quickstart)

### Windows

```powershell
irm https://claude.ai/install.ps1 | iex
```

### MacOS & Linux

```bash
curl -fsSL https://claude.ai/install.sh | bash
```

## 2. Install AWS CLI

Admin privileges are required for all platforms to install the AWS CLI.

* [Installing or updating to the latest version of the AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html)

### Windows

```powershell
msiexec.exe /i https://awscli.amazonaws.com/AWSCLIV2.msi
```

### MacOS

```bash
curl "https://awscli.amazonaws.com/AWSCLIV2.pkg" -o "AWSCLIV2.pkg"
sudo installer -pkg AWSCLIV2.pkg -target /
```

### Linux (x86_64)

```bash
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
sudo ./aws/install
```

### Linux (arm64)

```bash
curl "https://awscli.amazonaws.com/awscli-exe-linux-aarch64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
sudo ./aws/install
```

### Verify Installation

Run the following command to verify the AWS CLI installation:

```bash
aws --version
```

You should see output similar to:

```text
aws-cli/2.31.29 Python/3.13.9 Windows/11 exe/AMD64
```

or

```text
aws-cli/2.31.29 Python/3.13.9 Darwin/25.0.0 exe/arm64
```

## 3. Configure AWS Credentials

### Add Your IAM User Credentials

* [Authenticating using IAM user credentials for the AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/cli-authentication-user.html)

Get your credential files ready. You should have received one of this from Michael Wu. It will look something like

```csv
Access key ID,Secret access key
AKIAIOSFODNN7EXAMPLE,wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
```

Run the following command:

```bash
aws configure
```

The CLI will prompt you for four inputs. You should paste your keys for the first two, and **ignore** the last two by pressing Enter.

1. **AWS Access Key ID:** Paste your Key ID (e.g., `AKIAIOSFODNN7EXAMPLE`) and press **Enter**.
2. **AWS Secret Access Key:** Paste your Secret Key (e.g., `wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY`) and press **Enter**.
3. **Default region name:** Press **Enter** to skip (leave default).
4. **Default output format:** Press **Enter** to skip (leave default).

Your terminal interaction should look similar to this:

```bash
$ aws configure
AWS Access Key ID [None]: AKIAIOSFODNN7EXAMPLE
AWS Secret Access Key [None]: wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
Default region name [None]: 
Default output format [None]: 
```

### Verify the Configuration

Verify your AWS credentials are correctly configured:

```bash
aws sts get-caller-identity
```

You should see output similar to:

```json
{
    "UserId": "AKIAIOSFODNN7EXAMPLE",
    "Account": "SOME_NUMBER",
    "Arn": "arn:aws:iam::SOME_NUMBER:user/YOUR_NAME"
}
```

## 4. Configure the Models

Configure which Claude models Claude Code should use via AWS Bedrock.

### Check Available Models

First, check the available models in your region. This command lists Anthropic models provided by Bedrock.

* [List Available Models Documentation](https://docs.claude.com/en/api/claude-on-amazon-bedrock#list-available-models)
* [Supercharge your development with Claude Code and Amazon Bedrock prompt caching](https://aws.amazon.com/blogs/machine-learning/supercharge-your-development-with-claude-code-and-amazon-bedrock-prompt-caching/)
* [Claude Code Environment Variables](https://code.claude.com/docs/en/settings#environment-variables)

```bash
aws bedrock list-foundation-models --region=us-east-1 --by-provider anthropic --query "modelSummaries[*].modelId"
```

You should see a list of model IDs, for example:

```text
[
    "anthropic.claude-sonnet-4-20250514-v1:0",
    "anthropic.claude-haiku-4-5-20251001-v1:0",
    "anthropic.claude-sonnet-4-5-20250929-v1:0",
    "anthropic.claude-opus-4-1-20250805-v1:0",
    ...
]
```

**Remember to prepend `us.` to any model ID when configuring it (e.g., `us.anthropic.claude-sonnet-4-5-20250929-v1:0`).**

### Windows

Set environment variables for Claude Code to use Bedrock and specify default Sonnet and Haiku models. These are temporary for the current session.

```powershell
$Env:CLAUDE_CODE_USE_BEDROCK='1'
$Env:ANTHROPIC_DEFAULT_SONNET_MODEL='us.anthropic.claude-sonnet-4-5-20250929-v1:0' # Replace with desired Sonnet model
$Env:ANTHROPIC_DEFAULT_HAIKU_MODEL='us.anthropic.claude-haiku-4-5-20251001-v1:0'    # Replace with desired Haiku model
```

For persistent configuration, add these to your Windows Environment Variables settings.

### MacOS & Linux

Set environment variables for Claude Code to use Bedrock and specify default Sonnet and Haiku models.

Add these to your shell's startup script for persistent configuration:

**Zsh**

```zsh
echo "export CLAUDE_CODE_USE_BEDROCK=1" >> ~/.zshrc
echo "export ANTHROPIC_DEFAULT_SONNET_MODEL='us.anthropic.claude-sonnet-4-5-20250929-v1:0'" >> ~/.zshrc # Replace with desired Sonnet model
echo "export ANTHROPIC_DEFAULT_HAIKU_MODEL='us.anthropic.claude-haiku-4-5-20251001-v1:0'" >> ~/.zshrc    # Replace with desired Haiku model
```

**Bash**

```bash
echo 'export CLAUDE_CODE_USE_BEDROCK=1' >> ~/.bash_profile
echo "export ANTHROPIC_DEFAULT_SONNET_MODEL='us.anthropic.claude-sonnet-4-5-20250929-v1:0'" >> ~/.bash_profile # Replace with desired Sonnet model
echo "export ANTHROPIC_DEFAULT_HAIKU_MODEL='us.anthropic.claude-haiku-4-5-20251001-v1:0'" >> ~/.bash_profile    # Replace with desired Haiku model
```

After modifying the startup script, either close and reopen your terminal or source the script:

* **Zsh:** `source ~/.zshrc`
* **Bash:** `source ~/.bash_profile`

## 5. Congratulations

You have successfully installed and configured Claude Code to use AWS Bedrock.

You can now start using Claude Code by typing `claude` in your terminal:

```bash
claude
```

For my personal case on both Windows and MacOS, the Claude Code extension for VS Code is automatically installed after all the previous steps. It is also automatically configured to use the correct credentials and models from environment variable.
