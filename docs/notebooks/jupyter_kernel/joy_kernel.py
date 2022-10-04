import sys
from ipykernel.kernelbase import Kernel
from joy import (
    default_defs,
    initialize,
    inscribe,
    run,
    stack_to_string,
    )
#from joy.utils.pretty_print import trace


#inscribe(trace)


class FileFaker:

    def __init__(self):
        self.output = ''

    def write(self, text):
        self.output += text

    def flush(self):
        pass

    @classmethod
    def hook(class_, f):
        o = class_()
        sys.stdout, old_stdout = o, sys.stdout
        try:
            f()
        finally:
            sys.stdout = old_stdout
        return o.output


class JoyKernel(Kernel):
    implementation = 'Joypy'
    implementation_version = '1.0'
    language = 'Joy'
    language_version = '0.5'
    language_info = {
        'name': 'Joy',
        'mimetype': 'text/plain',
        'file_extension': '.joy',
    }
    banner = "Joy kernel"

    def __init__(self, *a, **b):
      self.D = initialize()
      default_defs(self.D)
      self.S = ()
      super(JoyKernel, self).__init__(*a, **b)

    def do_execute(
      self,
      code,
      silent,
      store_history=True,
      user_expressions=None,
      allow_stdin=False,
      ):
      def f():
        self.S = run(code, self.S, self.D)[0]
      output = FileFaker.hook(f)
      if not silent:
        text = stack_to_string(self.S)
        if output:
          text = '%s\n%s' % (output, text)
        stream_content = {
          'name': 'stdout',
          'text': text,
          }
        self.send_response(self.iopub_socket, 'stream', stream_content)

      return {'status': 'ok',
              # The base class increments the execution count
              'execution_count': self.execution_count,
              'payload': [],
              'user_expressions': {},
             }


if __name__ == '__main__':
  from ipykernel.kernelapp import IPKernelApp
  IPKernelApp.launch_instance(kernel_class=JoyKernel)
