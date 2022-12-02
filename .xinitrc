if command -v dbus-launch >/dev/null && test -z "${DBUS_SESSION_BUS_ADDRESS}"; then
       eval $(dbus-launch --sh-syntax --exit-with-session)
fi

# Ensure XDG_RUNTIME_DIR is set
unset XDG_RUNTIME_DIR
export XDG_RUNTIME_DIR=$(mktemp -d /tmp/$(id -u)-runtime-dir.XXX)

xrandr --auto &
exec sbcl --load .stumpwm.d/start.lisp
