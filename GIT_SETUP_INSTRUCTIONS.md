# Git Setup Instructions

## Problem
You initialized git in the wrong directory (`Repair/` instead of `lee-2004/`).

## Solution

### Option 1: Remove parent git repo and initialize in lee-2004 (Recommended)

```bash
# Navigate to parent directory
cd ~/OneDrive\ -\ student.uni-halle.de/Desktop/Repair

# Remove the git repo from parent directory
rm -rf .git

# Navigate to lee-2004 directory
cd lee-2004

# Initialize git in the correct directory
git init

# Add all files
git add .

# Check what will be committed
git status

# Make initial commit
git commit -m "Initial commit: Lee-Moretti-Butler (2004) RD replication - Professional portfolio piece"
```

### Option 2: If you want to keep parent repo, exclude Lee2024

If you need the parent directory to be a git repo, you can:

```bash
# In the parent directory, add to .gitignore
echo "Lee2024/" >> .gitignore

# Then try adding again
git add .
```

But for this project, **Option 1 is recommended** - initialize git only in `lee-2004/`.

## After Initial Commit

```bash
# Create repository on GitHub first, then:
git remote add origin https://github.com/YOUR_USERNAME/lee-2004.git
git branch -M main
git push -u origin main
```

