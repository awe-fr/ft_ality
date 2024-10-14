NAME = ft_ality

DUNE = opam exec -- dune

TARGET = _build/default/bin/main.exe

INIT = opam init -y && opam install dune -y && opam install tsdl -y

$(TARGET):
	$(DUNE) build
	cp ./_build/default/bin/main.exe ./main.exe
	mv ./main.exe ./ft_ality	

first:
	$(INIT)

clean:
	$(DUNE) clean
	rm -rf ./ft_ality

re: clean $(TARGET)

.PHONY: build clean re
