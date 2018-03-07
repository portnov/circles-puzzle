# circles puzzle

A relatively simple 2D puzzle with rotating circles.

It has three colored intersecting circles:

![Illustration](https://user-images.githubusercontent.com/284644/37111710-37abe0dc-2262-11e8-9f70-f4f4b08add17.png)

Each circle can be rotated by 60 degrees clockwise or counterclockwise.
Segments are moving correspondingly.  If you rotate each circle some random
number of times, segments will be moved to some random position.

The goal is to return segments into initial positions after that.

# Installation

```
$ sudo apt-get install stack
$ git clone https://github.com/portnov/circles-puzzle.git
$ cd circles-puzzle/
$ stack install
```

# Controls

Keyboard: letters `w`, `a`, `d` to rotate left, top and right circle clockwise;
`W`, `A`, `D` to rotate them counterclockwise.

Mouse: point over one of circles (thick border will indicate that circle is
selected) and press left mouse button to rotate it counterclockwise, or right
mouse button to rotate it clockwise.

Also standard controls to manipulate the viewport are available:

* Quit            
  - esc-key

* Move Viewport   
  - arrow keys
  - left-click drag

* Zoom Viewport
  - page up/down-keys
  - control-left-click drag
  - right-click drag
  - mouse wheel

* Rotate Viewport
  - home/end-keys
  - alt-left-click drag

* Reset Viewport
  r-key

