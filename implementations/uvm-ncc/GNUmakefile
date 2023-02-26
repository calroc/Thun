SRC_DIR=$(HOME)/src
UVM_JOY=$(SRC_DIR)/Joypy/implementations/uvm-ncc
UVM=$(SRC_DIR)/uvm
UVM_NCC=$(UVM)/ncc
UVM_VM=$(UVM)/vm

run: $(UVM_NCC)/out.asm
	cd $(UVM_VM) ; cargo run $(UVM_NCC)/out.asm

$(UVM_NCC)/out.asm: $(UVM_JOY)/xerblin.c
	cd $(UVM_NCC) ; cargo run $(UVM_JOY)/xerblin.c

clean:
	$(RM) $(UVM_NCC)/out.asm
