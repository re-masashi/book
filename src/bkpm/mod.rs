use std::fs::{self, File};
use std::io::Write;
use std::process::Command;

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct BkPackage {
    pub name: String,
    pub version: String,
    pub dependencies: Option<Vec<Dependency>>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Dependency {
    pub name: String,
    pub version: String,
    pub git: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Lockfile {
    pub package: Vec<LockedDep>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct LockedDep {
    pub name: String,
    pub git: String,
    pub rev: String,
}

pub fn init_package(name: &str) -> std::io::Result<()> {
    fs::create_dir_all(format!("{}/src", name))?;

    let main_path = format!("{}/src/main.bk", name);
    File::create(main_path)?.write_all(b"println(\"Hello, world!\")\n")?;

    let pkg = BkPackage {
        name: name.to_string(),
        version: "0.1.0".into(),
        dependencies: None,
    };

    let mut manifest = File::create(format!("{}/bkpm.toml", name))?;
    manifest.write_all(toml::to_string(&pkg).unwrap().as_bytes())?;

    println!("Initialized package `{}`", name);
    Ok(())
}

pub fn build_dependencies() -> std::io::Result<()> {
    let manifest_str = fs::read_to_string("bkpm.toml")?;
    let manifest: BkPackage = toml::from_str(&manifest_str).expect("invalid bkpm.toml");

    let mut locked = vec![];

    if let Some(deps) = manifest.dependencies {
        fs::create_dir_all("bkpm_modules")?;

        for dep in deps {
            if let Some(repo) = &dep.git {
                let clone_path = format!("bkpm_modules/{}", dep.name);
                if Path::new(&clone_path).exists() {
                    println!("Already exists: {}", dep.name);
                } else {
                    println!("Cloning {} from {}", dep.name, repo);
                    Command::new("git")
                        .args(["clone", "--depth=1", repo, &clone_path])
                        .status()
                        .expect("Failed to clone repo");
                }

                // Get the current HEAD commit hash
                let rev = get_git_commit_hash(&clone_path)?;
                locked.push(LockedDep {
                    name: dep.name.clone(),
                    git: repo.clone(),
                    rev,
                });
            } else {
                println!("Dependency `{}` has no git field!", dep.name);
            }
        }

        let lockfile = Lockfile { package: locked };
        let mut file = File::create("bkpm.lock")?;
        file.write_all(toml::to_string(&lockfile).unwrap().as_bytes())?;
    }

    Ok(())
}

fn get_git_commit_hash(path: &str) -> std::io::Result<String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(path)
        .args(["rev-parse", "HEAD"])
        .output()?;
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

use std::path::Path;

pub fn install_dependency(name: &str, git: &str, version: &str) -> std::io::Result<()> {
    let mut manifest: BkPackage =
        toml::from_str(&fs::read_to_string("bkpm.toml")?).expect("invalid bkpm.toml");

    // Avoid duplicate install
    if Path::new(&format!("bkpm_modules/{}", name)).exists() {
        println!("`{}` already installed. Skipping.", name);
        return Ok(());
    }

    // Validate repo (ping head commit)
    let validate = Command::new("git")
        .args(["ls-remote", git, "HEAD"])
        .output()?;
    if !validate.status.success() {
        panic!("Git repo `{}` does not exist or is inaccessible", git);
    }

    // Add to bkpm.toml
    let dep = Dependency {
        name: name.to_string(),
        version: version.to_string(),
        git: Some(git.to_string()),
    };
    match &mut manifest.dependencies {
        Some(deps) => {
            if !deps.iter().any(|d| d.name == name) {
                deps.push(dep);
            }
        }
        None => manifest.dependencies = Some(vec![dep]),
    }
    fs::write("bkpm.toml", toml::to_string_pretty(&manifest).unwrap())?;

    // Clone repo
    fs::create_dir_all("bkpm_modules")?;
    Command::new("git")
        .args(["clone", "--depth=1", git, &format!("bkpm_modules/{}", name)])
        .status()
        .expect("failed to clone repo");

    // Record commit
    let rev = get_git_commit_hash(&format!("bkpm_modules/{}", name))?;

    // Update lockfile
    let mut lock = if Path::new("bkpm.lock").exists() {
        toml::from_str::<Lockfile>(&fs::read_to_string("bkpm.lock")?).unwrap()
    } else {
        Lockfile { package: vec![] }
    };
    if !lock.package.iter().any(|p| p.name == name) {
        lock.package.push(LockedDep {
            name: name.to_string(),
            git: git.to_string(),
            rev,
        });
    }
    fs::write("bkpm.lock", toml::to_string_pretty(&lock).unwrap())?;

    println!("Installed `{}` from {}", name, git);

    // Recursively build transitive dependencies
    build_dependencies_from(&format!("bkpm_modules/{}", name))?;

    Ok(())
}

pub fn build_dependencies_from(path: &str) -> std::io::Result<()> {
    let manifest_path = format!("{}/bkpm.toml", path);
    if !Path::new(&manifest_path).exists() {
        return Ok(());
    }

    let manifest_str = fs::read_to_string(&manifest_path)?;
    let manifest: BkPackage = toml::from_str(&manifest_str).expect("invalid nested bkpm.toml");

    if let Some(deps) = manifest.dependencies {
        for dep in deps {
            if let Some(repo) = &dep.git {
                install_dependency(&dep.name, repo, &dep.version)?;
            } else {
                println!("Dependency `{}` has no git field", dep.name);
            }
        }
    }

    Ok(())
}
