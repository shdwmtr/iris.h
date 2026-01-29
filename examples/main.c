/** define the iris implementation */
#define IRIS_IMPLEMENTATION
#include "../iris.h"

int main(int argc, char **argv) {
  const char *filepath = argc > 1 ? argv[1] : "examples/main.iris";
  char *source = read_file(filepath);

  /** initialize event loop for async support */
  g_event_loop = event_loop_new();

  VM vm = {0};
  IrisProgram *prog = compile_with_path(source, filepath);

  /** execute main program (this may spawn async tasks) */
  execute(&vm, prog);

  /** run event loop to completion (handles async tasks) */
  if (g_event_loop->ready_count > 0 || g_event_loop->timers) {
    event_loop_run(g_event_loop, &vm, prog);
  }

  /** cleanup the vm */
  vm_cleanup(&vm);
  event_loop_free(g_event_loop);
  g_event_loop = NULL;
  free(source);
  prog_cleanup(prog);
  return 0;
}
