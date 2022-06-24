## Usage

To use this module, open up the Makefile, and edit the following command:

```
run_script:
    PYTHONPATH=. python functions_2A_compounds_largescale_indv_plates_Meilin.py \
        --long-df-list-file <path/to/your/long_df_list_csv_file/here.csv?
```

Filling your file name in as outlined above. Do not write the `<` / `>` in the actual file path.

With this in place, from the root directory of this repository (the same directory the Makefile exists
in), type the command:

`make run_script`

in the terminal window.

That's it!