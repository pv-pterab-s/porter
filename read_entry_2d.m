## -*- compile-command: "octave read_data.m"; -*-
function [A] = read_entry_2d(fp, type)
  dims = fread(fp, 4, "int64");
  strides = fread(fp, 4, "int64");
  offset = fread(fp, 1, "int64");
  A = fread(fp, dims(1) * dims(2), type);
  A = reshape(A, dims(1), dims(2));
endfunction
