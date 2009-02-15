compile:
	find -name \*.el -type f -exec emacsclient --eval "(byte-compile-file \"${PWD}/{}\")" ';'
