## -*- compile-command: "octave read_data.m"; -*-
fp = fopen("/tmp/transform")

out = read_entry_2d(fp, 'single');
in = read_entry_2d(fp, 'single');

fp = fopen("/tmp/transform-result")
out = read_entry_2d(fp, 'single');

imagesc(out);
waitforbuttonpress
