
dir_bin = "/Users/kuwahatakazuaki/Program/bin/"
program = PIMDformat
# +++ gfortran +++
fc = gfortran
#fcopt = -Wall -O3
fcopt =  -Wall -O3 -fbacktrace -fbounds-check
# +++ End gfortran +++
# objs    = PIMDformat.o

$(program):
	$(fc) PIMDformat.f90 -o $@
	cp $@ $(dir_bin)
	rm $@
	@echo -e '\e[34m Noraml termination!!!\e[m\n'


#all:
#	@echo $(fc)
#	@echo $(dir_bin)


