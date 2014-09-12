Testing MOSES subsampling over Reuters90
=======================================

Collections of tools to experiment MOSES subsampling on the Reuters90
archive.

Requirements
------------

* Git
* Haskell
* MOSES

Instructions
------------

1. Clone that repository, and go under it

    ```
    git clone https://github.com/ngeiswei/reuters_90cat.git
    cd reuters_90cat
    ```

2. Download and unpack the archive

    ```
    wget http://disi.unitn.it/moschitti/corpora/Reuters21578-Apte-90Cat.tar.gz
    tar xvzf Reuters21578-Apte-90Cat.tar.gz
    ```

3. Convert the archive into utf-8 format

    ```
    ./conver_to_utf-8.sh
    ```

4. Compile the programs

   ```
   ghc src/parse_reuters_archive.hs -O2
   ghc src/subsample.hs -O2
   ```

5. Configure the settings of an experiment
