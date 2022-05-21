# Physics Utilities for AABBs
This library efficiently handles collisions and overlaps for 2D AABBs (axis aligned bounding boxes). It can also handle 2D "sight". While it is built for an entity type from a personal library of mine, it can be adopted for any user type. It is designed to hide as many of its internals as possible.

Feel free at anytime to reach out with feedback. I am still learning many basics with the craft. So you'd be doing me a huge favor. :)

# Demo
Also, I've built a demo to show this library's capabilities.

[Here](https://ariajanke.github.io/aabbtdp/demo-wasm/bin/spa_out.html)

# How to Use It
There are two main features to this library. Sight and Physics, described separately below.

### Getting Started with Physics
- Prepare "entries" which you set boundaries, displacement, layer, and other options.
- Override the`tdp::EventHandler`class, with your own event functions.
- Create a handler, then set a collision matrix.
- Reuse and update your entries during the main game loop. :)

### Getting Started with Sight
- Prepare and add "entries" to a handler
- Call run, and get your results
- Rinse and repeat throughout your game

More information is available in the docs/include sources.

### Other notes
This wonderful tool was used to assist the creation of these markdown files: [StackEdit](https://stackedit.io/).
