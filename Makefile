build:
	ocaml pkg/build.ml native=true native-dynlink=true

test: build
	rm _build/src_test/ -rf
	ocamlbuild -use-ocamlfind -I src src_test/test_ppx_import.byte --

clean:
	ocamlbuild -clean

.PHONY: build test clean

release:
	@if [ -z "$(VERSION)" ]; then echo "Usage: make release VERSION=1.0.0"; exit 1; fi
	git checkout -B release
	sed -i 's/%%VERSION%%/$(VERSION)/' pkg/META
	git add .
	git commit -m "Prepare for release."
	git tag -a v$(VERSION) -m "Version $(VERSION)"
	git checkout @{-1}
	git branch -D release
	git push origin v$(VERSION)

.PHONY: release
