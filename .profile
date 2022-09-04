if [ -z "$GPGKEY" ] && [ -f "${HOME}/.gnupg/gpg.conf" ]; then
	export GPGKEY=`sed -nr 's/^\s*default-key\s+//p' "${HOME}/.gnupg/gpg.conf"`
fi
