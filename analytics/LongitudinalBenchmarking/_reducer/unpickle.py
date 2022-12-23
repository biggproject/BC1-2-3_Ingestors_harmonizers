import pickle


def read_pickle(string):
    pickle_data = pickle.loads(eval(string))
    return pickle_data
