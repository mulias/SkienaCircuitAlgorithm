# Skiena Circuit Algorithm

In section 6-2 of the Algorithm Design Manual Skiena gives a vague sketch for an algorithm by which two robotic arms work together to test circuit boards. I created a simulation framework in OCaml and implemented a minimum spanning tree solution, which runs upwards of 30% faster than the naive solution on large inputs. While that's great, I was hoping for something better, because the naive solution is pretty bad.

I'll be real, I no longer remember how I compiled this thing. I probably used the magic tool that comes with core.