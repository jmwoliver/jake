# jake

A tiny, function-based command runner.

Jake lets you define and run project-specific commands in a `Jakefile`.

```
build(type: {*debug|release}) {
  echo "Building in $type mode..."
}

test() {
  @build(debug)
  echo "Running tests..."
}
```

```sh
$ jake test
```

## Installation

### Pre-Built Binaries

*Hombrew coming soon*

### Build from Source

Jake is written in [Zig](https://ziglang.org/). To build from source:

```sh
git clone https://github.com/jmwoliver/jake.git
cd jake
zig build -Doptimize=ReleaseFast
```

The binary will be at `zig-out/bin/jake`. Move it somewhere in your `$PATH`:

```sh
cp zig-out/bin/jake /usr/local/bin/
```

## Quick Start

1. Create a `Jakefile` in your project root:

```
build() {
  echo "Building..."
  cargo build
}

test() {
  cargo test
}

fmt() {
  cargo fmt
}
```

2. List available targets:

```sh
$ jake
Available targets:

  build()
  fmt()
  test()
```

3. Run a target:

```sh
$ jake build
$ echo "Building..."
Building...
$ cargo build
   Compiling myproject v0.1.0
```

## Jakefile Syntax

### Functions

Functions are the basic building blocks. A function has a name, optional parameters, and a body of shell commands:

```
name() {
  shell commands here
}
```

Everything inside the braces is passed to the shell as-is.

### Parameters

Functions can accept parameters:

```
greet(name) {
  echo "Hello, $name!"
}
```

```sh
$ jake greet World
$ echo "Hello, World!"
Hello, World!
```

#### Parameters with Defaults

Mark a default value with `*` inside a constraint set:

```
build(mode: {*debug|release}) {
  echo "Building in $mode mode"
}
```

```sh
$ jake build           # Uses debug (the default)
$ jake build release   # Uses release
```

#### Skipping to Later Parameters

Use `_` as a placeholder to use the default and skip to later parameters:

```
test(arch: {arm64|*x86_64}, count) {
  echo "Testing on $arch, $count times"
}
```

```sh
$ jake test _ 5        # arch=x86_64 (default), count=5
$ jake test arch=_ 10  # Same result using named syntax
```

#### Constrained Parameters

Constrain parameters to a set of allowed values:

```
deploy(env: {dev|staging|prod}) {
  echo "Deploying to $env..."
}
```

If you pass an invalid value, jake tells you:

```sh
$ jake deploy local
error: invalid value 'local' for parameter 'env'
       allowed values: dev, staging, prod
```

#### Required vs Optional

- Parameters without constraints or defaults are **required**
- Parameters with a `*default` are **optional**
- Parameters with constraints but no `*` are **required** (must be one of the options)

```
# arch is required (no default), type has a default
build(arch: {arm64|x86_64}, type: {*debug|release}) {
  echo "Building $arch in $type mode"
}
```

### Variable Interpolation

Use `$variable` to reference parameters in your commands:

```
build(name) {
  echo "Building $name"
  echo "$name build complete"
}
```

Variables work inside quoted strings too:

```
greet(name) {
  echo "Hello, $name! Welcome to the build."
}
```

### Calling Other Functions

Use `@function()` to call another jake function:

```
build() {
  echo "Building..."
}

test() {
  @build()
  echo "Running tests..."
}
```

Pass arguments to called functions:

```
build(arch) {
  echo "Building for $arch"
}

test(arch) {
  @build($arch)
  echo "Testing on $arch"
}
```

### Conditionals

Use `if`/`else if`/`else` to branch based on parameter values:

```
build(type: {*debug|release|small}) {
  if $type == debug {
    zig build
  } else if $type == release {
    zig build -Doptimize=ReleaseFast
  } else {
    zig build -Doptimize=ReleaseSmall
  }
}
```

Supported operators: `==` and `!=`

```
deploy(env: {dev|staging|prod}) {
  if $env != prod {
    echo "Deploying to test environment..."
  } else {
    echo "Deploying to PRODUCTION!"
  }
}
```

### Comments

Lines starting with `#` are comments:

```
# This is a comment
build() {
  # Another comment
  echo "Building..."
}
```

## Command Line Usage

```
jake [OPTIONS] [TARGET] [ARGS...]
```

### Options

| Option | Description |
|--------|-------------|
| `-f <FILE>` | Use a specific Jakefile |
| `-h, --help` | Show help message |

### Passing Arguments

**Positional arguments** are assigned in order:

```sh
# For: build(arch, type)
$ jake build arm64 release
```

**Named arguments** use `name=value` syntax:

```sh
$ jake build type=release arch=arm64
```

**Default placeholder** uses `_` to skip a parameter and use its default:

```sh
# For: test(arch: {arm64|*x86_64}, count)
$ jake test _ 5           # Use default arch, set count=5
$ jake test arch=_ 10     # Same with named syntax
```

You can mix positional and named arguments:

```sh
$ jake build arm64 type=release
```

### Examples

```sh
# List all targets
jake

# Run a target
jake build

# With positional arguments
jake test arm64 5

# With named arguments
jake deploy env=staging

# Skip to later parameters using _ placeholder
jake test _ 10

# Use a different Jakefile
jake -f build.jake test
```

## Features

### Function Composition

Build complex workflows by composing functions:

```
clean() {
  rm -rf dist/
}

build() {
  @clean()
  mkdir -p dist
  go build -o dist/app
}

test() {
  @build()
  go test ./...
}

release() {
  @test()
  tar -czf release.tar.gz dist/
}
```

### Constrained Parameters

Get validation for free. No more typos silently breaking builds:

```
deploy(env: {dev|staging|prod}) {
  ./deploy.sh --env=$env
}
```

```sh
$ jake deploy production
error: invalid value 'production' for parameter 'env'
       allowed values: dev, staging, prod
```

### Helpful Error Messages

Jake provides clear, colorized error messages with source context:

```
error: undefined function
  --> Jakefile:5:3
   |
 5 |   @bild($arch)
   |    ^^^^
```

## Comparison with Other Tools

| Feature | jake | make | just |
|---------|------|------|------|
| Function parameters | Yes | No | Yes |
| Constrained parameter values | Yes | No | No |
| Default parameter values | Yes | No | Yes |
| Call other tasks | `@task()` | Dependencies | Dependencies |
| Conditionals | Yes | Limited | No |

## License

MIT
