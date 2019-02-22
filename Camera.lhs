In order to build a raytracer we need some concept of where we are viewing the
world from. This consists of several pieces, the focal point of the camera, the
field of view, the direction the camera is pointing, and finally where the top
of the camera faces. With all of these components together we can generate a
list of rays, each one representing a pixel of our camera. Then in another
module we will take the list of rays and generate our image from it.
