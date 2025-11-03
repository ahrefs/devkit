# HtmlStream GC Benchmark Suite

This benchmark suite is designed to stress-test the OCaml garbage collector using the HtmlStream HTML parser. The benchmarks create various allocation patterns and heap shapes to exercise different aspects of GC behavior.

## Building

```bash
# Build the benchmark executable
dune build htmlStream_bench.exe

# Or build everything including the benchmark
make build
```

## Running

```bash
# Run all benchmarks with default settings
dune exec ./htmlStream_bench.exe

# Or using the bench alias
dune build @bench

# Run with custom parameters
./htmlStream_bench.exe -warmup 5 -iterations 20 -verbose

# Run without GC statistics
./htmlStream_bench.exe -no-gc-stats
```

## Command-line Options

- `-warmup N`: Number of warmup iterations (default: 3)
- `-iterations N`: Number of benchmark iterations (default: 10)
- `-verbose`: Enable verbose output
- `-no-gc-stats`: Disable GC statistics output

## Benchmarks

The suite contains 8 benchmarks, each targeting different GC behaviors:

### 1. Small String Pressure
- **Target**: Minor GC stress
- **Pattern**: Creates many small text nodes (~10-20 chars each)
- **Allocation**: High frequency of small string allocations via `String.sub`
- **GC Impact**: Frequent minor collections, high allocation rate
- **Heap Shape**: Many short-lived objects in minor heap

### 2. Attribute List Pressure
- **Target**: List allocation and reversal overhead
- **Pattern**: Tags with 20-50 attributes each
- **Allocation**: List cons cells and reversed lists
- **GC Impact**: Mixed minor/major pressure from list structures
- **Heap Shape**: Intermediate-lived linked structures

### 3. Large Block Allocations
- **Target**: Major heap and large object handling
- **Pattern**: Script/style blocks of 1KB-100KB
- **Allocation**: Large string allocations
- **GC Impact**: Major heap pressure, potential compaction triggers
- **Heap Shape**: Mix of large objects, tests heap expansion

### 4. Morphing Heap Shape
- **Target**: GC adaptation to changing patterns
- **Pattern**: Alternates between small texts, deep nesting, and large blocks
- **Allocation**: Varies between allocation patterns in phases
- **GC Impact**: Tests GC heuristics and adaptation
- **Heap Shape**: Continuously changing object size distribution

### 5. Heap Fragmentation
- **Target**: Heap fragmentation and compaction
- **Pattern**: Objects of sizes [10, 100, 1000, 10000] bytes in varying order
- **Allocation**: Non-uniform size distribution
- **GC Impact**: Fragmentation leading to compaction
- **Heap Shape**: Fragmented free lists, gaps between live objects

### 6. Generational Hypothesis Violation
- **Target**: Inter-generational pointers
- **Pattern**: Updates old objects to reference new ones
- **Allocation**: Violates assumption that old objects rarely point to new
- **GC Impact**: Increased write barrier overhead, remembered set pressure
- **Heap Shape**: Complex inter-generational reference patterns

### 7. Variable Allocation Rate
- **Target**: GC pacing and scheduling
- **Pattern**: Sine wave pattern of allocation intensity
- **Allocation**: Rate varies from low to high in waves
- **GC Impact**: Tests GC triggering heuristics
- **Heap Shape**: Periodic growth and shrinkage

### 8. Complex Reference Graph
- **Target**: GC marking phase performance
- **Pattern**: Interconnected graph structure with cross-references
- **Allocation**: Complex pointer relationships
- **GC Impact**: Deep marking traversal, potential mark stack overflow
- **Heap Shape**: Dense reference graph requiring deep traversal

## Interpreting Results

Each benchmark reports:
- **Execution time**: Total time for all iterations
- **Throughput**: Iterations per second
- **Minor words allocated**: Total allocation in minor heap
- **Promoted words**: Data promoted to major heap
- **Major words allocated**: Direct major heap allocation
- **Minor collections**: Number of minor GC cycles
- **Major collections**: Number of major GC cycles
- **Heap words**: Current heap size
- **Live words**: Currently live data
- **Fragments**: Heap fragmentation count
- **Compactions**: Number of heap compactions

## GC Tuning

The benchmarks set specific GC parameters for reproducibility:
- Minor heap size: 256KB
- Major heap increment: ~124KB

You can modify these in the code or use `OCAMLRUNPARAM` environment variable:

```bash
# Increase minor heap to reduce minor GC frequency
OCAMLRUNPARAM='s=512k' ./htmlStream_bench.exe

# Enable verbose GC output
OCAMLRUNPARAM='v=0x205' ./htmlStream_bench.exe

# Custom space overhead (default 120)
OCAMLRUNPARAM='o=150' ./htmlStream_bench.exe
```

## Use Cases

This benchmark suite is useful for:

1. **GC Algorithm Development**: Testing new GC algorithms or heuristics
2. **Performance Regression Testing**: Detecting GC performance regressions
3. **Memory Profiling**: Understanding allocation patterns in HTML processing
4. **GC Tuning**: Finding optimal GC parameters for HTML-heavy workloads
5. **Research**: Studying GC behavior under various allocation patterns

## Expected Behavior

Under normal conditions with OCaml's default GC:

- **Small String Pressure**: Many minor collections, low major collections
- **Attribute Lists**: Moderate minor and major collections
- **Large Blocks**: Fewer but longer major collections, possible compactions
- **Morphing Heap**: Variable collection frequency following pattern changes
- **Fragmentation**: Increasing fragment count leading to compaction
- **Generational**: Higher promotion rate than typical
- **Variable Rate**: Collection frequency tracks allocation rate
- **Complex References**: Longer marking phases in major collections

## Extending the Suite

To add new benchmarks:

1. Create a new function following the pattern:
```ocaml
let bench_my_pattern config =
  let generate_html () = (* ... *) in
  let process_elem elem = (* ... *) in
  run_benchmark ~config ~name:"My Pattern" ~generate_html ~process_elem ()
```

2. Add to the `run_all_benchmarks` function

3. Focus on specific GC stress patterns:
   - Allocation size distribution
   - Object lifetime patterns
   - Reference complexity
   - Heap shape evolution