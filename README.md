1. harness the arrayfire directory
  - copies opencl driver to oneapi driver
2. C-c 1 switch to opencl kernel
3. move cursor to inside a kernel
4. C-c 2 make a functor
5. C-c 3 switch to oneapi driver
6. paste the functor after the write_accessor definition
7. move the cursor to a driver function
8. C-c 4 make a driver function
9. paste driver function above old driver function. delete old one
