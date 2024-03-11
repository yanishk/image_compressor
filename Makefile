##
## EPITECH PROJECT, 2024
## wolfram
## File description:
## Makefile
##

EXECUTABLEPATH = $(shell stack path --local-install-root)

EXECUTABLEDIR = bin

EXECUTABLENAME = imageCompressor-exe

NAME = imageCompressor

RM = rm -f

all:
	stack build
	cp $(EXECUTABLEPATH)/$(EXECUTABLEDIR)/$(EXECUTABLENAME) ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re:	 fclean all

.PHONY = all clean fclean re
