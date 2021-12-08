# MINI GEOGEBRA

This project aims to reproduce basic features of Geogebra.

Almost all interactions with the software will use an emulated command line in the browser
(the bootstrap project will provide a skeleton for this).

The evaluation will mainly focus on code quality and features.
Efforts on UI will NOT be taken into account, so don't spend too much time on it.
(That said, in "real" projects, UI/UX really is important and you have to polish it
as much as possible).

## Minimal commands set
Objects should be displayed with their name next to them.
* add a point: `A = (1, 3)` (up to you to decide whether you allow multi chars name or restrict the names to a single char).
  Hint: using a `Float` might way easier than `Int` to do some computation later.
* add a segment between two existing points: `s = [A,B]`
* add a circle given the center (existing point) and a radius: `C = circle(A, 5)`
* allow segment creation directly with coordinate: `s= [(1,2), (5,3)]`. Same thing with the circle: `C = circle((1,3), 5)`

## Additional optional features
* bold shape on hover.
* intersection (this part requires some geometric skills. You can use some formulea found on the internet without deeply
     understanding them. The evaluation won't be on your math skills):
    - add a (infinite) line: `d = (A, B)` (where `A` and `B` are points). Hint: reprsent the line as an equation `ax +by + c = 0` 
      by only storing `a`, `b` and `c`.
      [Note: at this point, we should able to easily either use an already defined point or defining "on the fly" with coordinates]
    - compute lines intersection point: `P = inter(d1, d2)`. Issue an error if `d1` and `d2` don't cross.
    - extend the `inter` function to work with:  "circle and line", "circle and circle". You'll need to also extend calling syntax to 
      something like: `A, B = inter(d1, C)` to handle cases where there are multiple intersection points. Issue an error if 
      the actual number of intersection points doesn't match the asked number of points.
* a bit of scripting:
    - make it possible to store numeric values in variables, like: `i = 10`.
    - make it possible to create a shape without giving it a name, i.e., instead of `A=(1,3)`, the user would simply write `(1,3)`.
      The shape will be displayed on the screen but the user won't be able to reference it to build other objects from it.
    - Implement a for loop: `for i in 1..10: (i,i)` will display 10 (unnamed) dots on the screen (`(1, 1)`, `(2, 2)`, ...).   
* make it possible for the user to save the current session as a file and to restore the session by loading the file.
  *Hint: the whole state of the session only depends on the user commands.*


