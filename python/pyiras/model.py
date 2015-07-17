from network import IRASNetwork
from parameters import parameter_register


class IRASModel(object):
    def __init__(self, network=None):
        self.network = network

    @classmethod
    def from_inp_file(cls, filename):

        print 'Loading network...'
        network = IRASNetwork.from_inp_file(filename)
        print 'Loading parameters...'
        cls._load_parameters_from_inp(filename, network)

        return cls(network=network)

    @classmethod
    def _load_parameters_from_inp(cls, filename, network):
        '''Read parameters from inp file
        '''
        with open(filename, 'r') as fh:

            row = fh.readline()
            while not row.startswith('END OF LINKS'):
                row = fh.readline()


            row = fh.readline()
            while row:
                # Skip comments
                if row.startswith('!'):
                    row = fh.readline()
                    continue

                data = row.strip()
                # Remove inline comments
                if '!' in row:
                    data, comment = data.split('!', 1)

                if ':' not in data:
                    row = fh.readline()
                    continue

                parameter, values = data.split(':', 1)
                values = values.split()
                try:
                    cls = parameter_register[parameter]
                except KeyError:
                    print("Warning: Parameter '{}' not understood.".format(parameter))
                    row = fh.readline()
                    continue
                    
                try:
                    cls.load_from_inp_data(values, network)
                except:
                    print("Warning: Failed to load parameter '{}' with row: '{}'".format(parameter, row))

                row = fh.readline()

if __name__ == '__main__':

    data_dir = '../../london'

    from os.path import join

    N = IRASModel.from_inp_file(join(data_dir, 'iras.inp'))


    N.network.draw_with_mpl()
    import matplotlib.pyplot as plt

    plt.show()
