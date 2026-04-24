# Instructions

Run the script `run.sh`, which sets up a simulated network
of a client and server node, and run the simulation.

The output consists of four files:
- small_nonincremental
- small_incremental
- large_nonincremental
- large_incremental

Which capture durations of downloading a piece of data (idle time)
and decoding it.

The script `parse.hs` can be ran to further summarize all these results,
which is especially useful for the incremental decoder runs, since
the logs contain many prints for each SDU-sized chunk processed.
