# Don't Use it!

This is a repo only for backup, which means doesn't use any more, just for memory. The code is the extraction for EECS590 project version.

Extraction from Hazel to OCaml, directly extract from ```UHExp.t```. The ```hazel-extraction``` folder stores all files necessary for Hazel, and ```extraction_history``` is the stopped rewrite structure for that.

The work of formal definitions of the extraction is in the pdf file.


The version of hazel still supports gradual type (different output type for case). It also means that hazel is an old version, so maybe new environment will fail to make. Maybe prepare a new opam is great.

The compiled file is copied to ```www``` folder, manually open the index. 

Note that the version has some problems. Probably lies in bad ```UNK``` pass check and ```Letline```, because a block's type is only decided by the expression line.
