Common Lisp translation of the [hellovr_opengl](https://github.com/ValveSoftware/openvr/blob/master/samples/hellovr_opengl/hellovr_opengl_main.cpp) from Valve's [OpenVR SDK](https://github.com/ValveSoftware/openvr).

CL OpenVR bindings: https://github.com/3b/3b-openvr

Uses [glop](https://github.com/lispgames/glop) for window creation and
GL context, though tried to keep it separate to allow adding SDL
version later.