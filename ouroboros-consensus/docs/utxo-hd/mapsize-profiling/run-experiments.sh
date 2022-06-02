set -x

PROJECT_NAME=mapsize-profiling

exec > >(tee -i ./$PROJECT_NAME.log)
exec 2>&1

EXP_DIR=./experiment
DB_DIR=./dbs
RESULT_DIR=./res

if [ ! -d "$EXP_DIR" ]; then
  mkdir $EXP_DIR
fi

if [ -d "$EXP_DIR/$DB_DIR" ]; then
  rm -r "$EXP_DIR/$DB_DIR"
fi
mkdir "$EXP_DIR/$DB_DIR"

if [ -d "$EXP_DIR/$RESULT_DIR" ]; then
  rm -r "$EXP_DIR/$RESULT_DIR"
fi
mkdir "$EXP_DIR/$RESULT_DIR"

# === Example experiments ===

if false; then
  EXP=EXP0.1
  mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
  cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "[1024, 1024]" file "./inputs/pgs-k1.txt" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
  hp2ps "$PROJECT_NAME.hp"
  mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
  mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
  mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
  mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"

  EXP=EXP0.2
  mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
  cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "[1024, 1024]" cli "[Puts 1 1000]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
  hp2ps "$PROJECT_NAME.hp"
  mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
  mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
  mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
  mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"
fi

# === Actual experiments ===

GB1="[1024, 1024, 1024]"
GB32="[1024, 1024, 1024, 32]"

# 1GB map size, no commands
EXP=EXP1
mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "$GB1" cli "[]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
hp2ps "$PROJECT_NAME.hp"
mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"

# 32GB map size, no commands
EXP=EXP2
mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "$GB32" cli "[]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
hp2ps "$PROJECT_NAME.hp"
mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"

# 1GB map size, 1 million puts
EXP=EXP3
mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "$GB1" cli "[Puts 1 1000000]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
hp2ps "$PROJECT_NAME.hp"
mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"

# 32GB map size, 1 million puts
EXP=EXP4
mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "$GB32" cli "[Puts 1 1000000]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
hp2ps "$PROJECT_NAME.hp"
mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"

# 1GB map size, 1 million puts, 1 million gets
EXP=EXP5
mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "$GB1" cli "[Puts 1 1000000, Gets 1 1000000]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
hp2ps "$PROJECT_NAME.hp"
mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"

# 32GB map size, 1 million puts, 1 million gets
EXP=EXP6
mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "$GB32" cli "[Puts 1 1000000, Gets 1 1000000]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
hp2ps "$PROJECT_NAME.hp"
mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"

# 1GB map size, 10 million puts
EXP=EXP7
mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "$GB1" cli "[Puts 1 10000000]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
hp2ps "$PROJECT_NAME.hp"
mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"

# 32GB map size, 10 million puts
EXP=EXP8
mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "$GB32" cli "[Puts 1 10000000]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
hp2ps "$PROJECT_NAME.hp"
mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"


# 1GB map size, 10 million puts, 10 million gets
EXP=EXP9
mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "$GB1" cli "[Puts 1 10000000, Gets 1 10000000]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
hp2ps "$PROJECT_NAME.hp"
mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"

# 32GB map size, 10 million puts, 10 million gets
EXP=EXP10
mkdir "$EXP_DIR/$RESULT_DIR/$EXP"
cabal run $PROJECT_NAME -- "$EXP_DIR/$DB_DIR/$EXP" "$GB32" cli "[Puts 1 10000000, Gets 1 10000000]" 2>&1 | tee "$EXP_DIR/$RESULT_DIR/$EXP/shell.log"
hp2ps "$PROJECT_NAME.hp"
mv $PROJECT_NAME.hp "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.hp"
mv $PROJECT_NAME.ps "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.ps"
mv $PROJECT_NAME.aux "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.aux"
mv $PROJECT_NAME.prof "$EXP_DIR/$RESULT_DIR/$EXP/$PROJECT_NAME.prof"

# Analyse database sizes
du -sh $EXP_DIR/$DB_DIR/* 2>&1 | tee "$EXP_DIR/$RESULT_DIR/dbsizes.log"