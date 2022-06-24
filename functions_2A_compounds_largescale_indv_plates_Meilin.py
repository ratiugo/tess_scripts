"""
General description of module/script goes here
"""
import pandas as pd
import argparse


class Pipeline:
    """
    You should change this name to something more descriptive, along the lines of
    CompoundsLargescalePipeline.

    General description of pipeline
    """
    def __init__(self, **kwargs):
        with open(kwargs.get('long_df_list_file')) as long_df_list_file:
            self.long_df_list = pd.read_csv(long_df_list_file)


    def run(self):
        """
        Run the pipeline. This will call each method/transformation of the dataframe
        """
        return (self.filter_long_df_list())


    def filter_long_df_list(self):
        """
        Small function from lines 24 - 34 for now
        """
        print(self.long_df_list)



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--long-df-list-file',
                        dest='long_df_list_file')

    Pipeline(**vars(parser.parse_args())).run()
