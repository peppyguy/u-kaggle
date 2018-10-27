import kaggle
import pandas as pd

API = kaggle.api
PAGE_SIZE = 100
END = 1200
COULMNS = ['author', 'categoryIds', 'competitionDataSources',
           'datasetDataSources', 'enableGpu', 'enableInternet', 'id',
           'isPrivate', 'kernelDataSources', 'kernelType', 'language',
           'lastRunTime', 'ref', 'slug', 'title', 'totalVotes']

DATAFRAME = pd.DataFrame(columns=COULMNS)


for idx in range(0, END, PAGE_SIZE):
    print(f"\rWorking on {idx}...{idx+PAGE_SIZE} of {END}", end="")

    page = idx//PAGE_SIZE + 1
    kernels = API.kernels_list(page=page, page_size=PAGE_SIZE)

    rows = [dict((column, getattr(kernel, column)) for column in COULMNS) for
            kernel in kernels]

    try:
        DATAFRAME = DATAFRAME.append(rows)
    except IndexError:
        print("Done!")
        break

DATAFRAME.to_csv('kernel_list.csv', index=False)
DATAFRAME['ref'].to_csv('kernel_refs.csv', index=False)
