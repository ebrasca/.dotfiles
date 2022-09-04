xrandr --auto &
exec dbus-launch --exit-with-session sbcl --load .stumpwm.d/start.lisp ; gentoo-pipewire-launcher
