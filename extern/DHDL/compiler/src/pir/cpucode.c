#include "stdlib.h"
#include "stdio.h"
#include "stdint.h"
#include "sys/time.h"
#include "Maxfiles.h"
#include "MaxSLiCInterface.h"
#include "types.h"

//uint32_t burstSize = 384;
//int32_t nextLMemAddr = burstSize * 1024;
//uint32_t getNextLMemAddr(uint32_t size) {
//  uint32_t addr = nextLMemAddr;
//  nextLMemAddr += size + (burstSize - size%burstSize);
//  return addr;
//}

float *parser(char* name, int size)
{
  float *arr = malloc(size * sizeof(float));
  FILE *fp;
  char *line = NULL;
  size_t len = 0;
  long i = 0;
  ssize_t read;
  fp=fopen(name,"r");
  if(fp != NULL)
  {
    while ((read = getline(&line, &len, fp)) != -1) {
      arr[i] = atof(line);
      i = i + 1;
    }
    fclose(fp);
  }
  else {
    printf("ERROR: NO DATA");
    for (i=0; i<size; i++) {
      arr[i] = (float)1;
    }
  }   
  return arr;
}


int main(int argc, char **argv)
{
uint32_t i=0, j=0;
struct timeval t1, t2;

if (argc != (2+1)) {
  fprintf(stderr, "Usage: %s <argIn_M> <argIn_B>\n\n", argv[0]);
  exit(-1);
}

uint32_t argIn_M = atoi(argv[1]);
uint32_t argIn_B = atoi(argv[2]);
uint64_t cycles = (uint64_t) 0;
uint32_t offchip_train_y_263_size = argIn_B;
uint32_t B = argIn_B;
float *offchip_train_y_263 = parser("/kunle/users/mattfel/maxj/dataY","r");

const uint32_t offchip_train_y_263_lmem_addr = 805306368;
uint32_t offchip_model_255_size = argIn_M;
float *offchip_model_255 = parser("/kunle/users/mattfel/maxj/dataM","r");

const uint32_t offchip_model_255_lmem_addr = 402653184;
uint32_t offchip_train_x_259_size = argIn_B*argIn_M;
float *offchip_train_x_259 = parser("/kunle/users/mattfel/maxj/dataX","r");

const uint32_t offchip_train_x_259_lmem_addr = 1610612736;
uint32_t offchip_result_369_size = argIn_M;
float *offchip_result_369 = malloc(offchip_result_369_size * sizeof(float));
for (i=0; i<offchip_result_369_size; i++) {
offchip_result_369[i] = (float)0;
}
const uint32_t offchip_result_369_lmem_addr = 1207959552;

// Load max file from disk -> DRAM
max_file_t *maxfile = Top_init();
// Configure the FPGA
max_engine_t *engine = max_load(maxfile, "local:*");
int burstSize = max_get_burst_size(maxfile, NULL);
printf("Burst size for architecture: %d bytes\n", burstSize);


// Transfer DRAM -> LMEM
Top_writeLMem_actions_t offchip_train_y_263_wrAct;
offchip_train_y_263_wrAct.param_size = offchip_train_y_263_size * sizeof(float);
offchip_train_y_263_wrAct.param_start = offchip_train_y_263_lmem_addr;
offchip_train_y_263_wrAct.instream_fromcpu = offchip_train_y_263;
Top_writeLMem_run(engine, &offchip_train_y_263_wrAct);


// Transfer DRAM -> LMEM
Top_writeLMem_actions_t offchip_model_255_wrAct;
offchip_model_255_wrAct.param_size = offchip_model_255_size * sizeof(float);
offchip_model_255_wrAct.param_start = offchip_model_255_lmem_addr;
offchip_model_255_wrAct.instream_fromcpu = offchip_model_255;
Top_writeLMem_run(engine, &offchip_model_255_wrAct);


// Transfer DRAM -> LMEM
Top_writeLMem_actions_t offchip_train_x_259_wrAct;
offchip_train_x_259_wrAct.param_size = offchip_train_x_259_size * sizeof(float);
offchip_train_x_259_wrAct.param_start = offchip_train_x_259_lmem_addr;
offchip_train_x_259_wrAct.instream_fromcpu = offchip_train_x_259;
Top_writeLMem_run(engine, &offchip_train_x_259_wrAct);


// Run kernel on FPGA
fprintf(stderr, "Running Kernel\n");
Top_actions_t runAct;

runAct.param_argIn_M = argIn_M;
runAct.param_argIn_B = argIn_B;
runAct.outscalar_TopKernel_cycles = &cycles;

gettimeofday(&t1, 0);
Top_run(engine, &runAct);
gettimeofday(&t2, 0);
double elapsed = (t2.tv_sec-t1.tv_sec)*1000000 + t2.tv_usec-t1.tv_usec;
fprintf(stderr, "Kernel done, elapsed time = %lf\n", elapsed/1000000);


// Transfer LMEM -> DRAM
// (sizeInBytes, address, dstptr)
Top_readLMem_actions_t offchip_result_369_rdAct;
offchip_result_369_rdAct.param_size = offchip_result_369_size *sizeof(float);
offchip_result_369_rdAct.param_start = offchip_result_369_lmem_addr;
offchip_result_369_rdAct.outstream_tocpu = offchip_result_369;
fprintf(stderr, "Starting FPGA -> CPU copy\n");
Top_readLMem_run(engine, &offchip_result_369_rdAct);
fprintf(stderr, "FPGA -> CPU copy done\n");


fprintf(stderr, "Printing offchip_result_369 to file\n");
FILE *outfile_offchip_result_369 = fopen("outfile_offchip_result_369.txt", "w");
uint32_t offchip_result_369_iter_0 = 0;
for (offchip_result_369_iter_0 = 0; offchip_result_369_iter_0 < argIn_M; offchip_result_369_iter_0++) {
uint32_t addr = +offchip_result_369_iter_0;
fprintf(outfile_offchip_result_369, "%f ", offchip_result_369[addr]);
} // offchip_result_369_iter_0
fprintf(outfile_offchip_result_369,"\n");
FILE *cycles_out = fopen("cycles_out.txt", "w");
fprintf(cycles_out, "cycles = %lu\n", cycles);
fclose(cycles_out);

  // Cleanup
free(offchip_train_y_263);
free(offchip_model_255);
free(offchip_train_x_259);
free(offchip_result_369);
max_unload(engine);
fprintf(stderr, "Exiting\\n");
return 0;
}

