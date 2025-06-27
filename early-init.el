(setenv "LIBRARY_PATH" "/nix/store/z12rd3v6wa1lwacgq58qvql0dldg4gm1-libgccjit-13.3.0/lib:/opt/local/lib/gcc14:/opt/local/lib/libgcc:/opt/local/lib/gcc14/gcc/aarch64-apple-darwin24/14.2.0/")
(setenv "LD_LIBRARY_PATH" "/nix/store/z12rd3v6wa1lwacgq58qvql0dldg4gm1-libgccjit-13.3.0/lib:/opt/local/lib/gcc14:/opt/local/lib/libgcc:/opt/local/lib/gcc14/gcc/aarch64-apple-darwin24/14.2.0/")
(if (eq system-type 'darwin) (add-to-list 'default-frame-alist '(undecorated . t)))
(setq byte-compile-warnings '(cl-functions))
