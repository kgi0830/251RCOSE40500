#include <stdlib.h>
#include <iostream>
#include <verilated.h>
#include <verilated_vcd_c.h>	// required for generating waveforms
#include "Vand2.h"

#define ITER 4

int main(int argc, char** argv, char** env) {
	Vand2 *dut = new Vand2;

	// initializing waveform dump
	Verilated::traceEverOn(true);
	VerilatedVcdC *m_trace = new VerilatedVcdC;
	dut->trace(m_trace, 5);
	m_trace->open("wave.vcd");

	// test vectors
	for (int i = 0; i < ITER; i++) {
		dut->a = 3;
		dut->b = ITER-1-i;
		dut->eval();
		m_trace->dump(i);
		printf("a: %d, b: %d, c: %d\n", dut->a, dut->b, dut->c);
	}
	dut->eval();
	m_trace->dump(ITER);

	// closing
	m_trace->close();
	delete dut;
	exit(EXIT_SUCCESS);
}
