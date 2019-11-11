library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(tidyverse)

# funckja wybierająca pliki.csv i łącząca je w jedną tabelę, działa dla dwóch plików.csv, 
# filtruje wartości po q, wybiera dwie kolummny sequence i protein.id
load_files_table <- function(path, fragment, q){
  file_s <- list.files(path, pattern = fragment)
  table1 <- read.table(paste0(path, "/", file_s[1]), header = TRUE, sep = ",")
  table2 <- read.table(paste0(path, "/", file_s[2]), header = TRUE, sep = ",")
  table <- rbind(table1, table2) # łączenie tabel w jedno, jedna pod drugą
  table <- filter(table, percolator.q.value < q) # filtrowanie
  table <- select(table, sequence, protein.id) # wybieranie potrzebnych kolumn
} 

table <- load_files_table(".", ".csv", 0.01)
# dim(table)  
# colnames(table) 
# head(table)

# rozseparowanie kolumny protein.id na protein_1 i protein_2
table_separ <- separate(table, protein.id, c("protein_1", "protein_2")) 

# utworzenie nowej tabeli table_1 o kolumnach sequence i protein_1
table_1 <- select(table_separ, sequence, protein_1)

# odrzucenie wartości <NA> i przypisanie wartości do nowej tabeli table_2 
# o kolumnach sequence i protein_2
table_2 <- filter(select(table_separ, sequence, protein_2), protein_2 != "<NA>") 

# zmiana nazw kolumn obu tabel table_1 i table_2 na nazwy kolumn tabeli table, czyli sequence i protein.id
colnames(table_1) <- colnames(table)
colnames(table_2) <- colnames(table)

# połączenie tabeli table_1 i table_2 (jedna pod drugą) oraz usunięcie powtórzonych wartości
table_protein <- rbind(table_1, table_2) 
dim(table_protein)
table_protein <- unique(table_protein) # tylko wartości unikalne

#=============================(I sposób na tworzenie tabeli peptydów)================================================
# tworzenie data frame zer i jedynek 
data_protein <- as.data.frame.matrix(table(table_protein))
data_protein[1:5, 1:5]

# zamiana data frame na macierz
matrix_protein <- as.matrix(data_protein)
colnames(matrix_protein) <- NULL
rownames(matrix_protein) <- NULL

# tworzenie macierzy ilości peptydów
matrix_peptid <- t(matrix_protein) %*% matrix_protein

# zamiana macierzy peptydów na tabelę peptydów i utworzenie nazw kolumn
table_peptid <- data.frame(diag(matrix_peptid))
class(table_peptid)
table_peptid <- mutate(table_peptyd, proteins = colnames(data_protein))
table_peptid[ , c(1,2)] <- table_peptid[ , c(2,1)] # zamiana kolumn
colnames(table_peptid) <- c("proteins", "qty_peptids")
head(table_peptid)

#=====================================================================================================

# filtrowanie po nazwie białek
search_proteins <- function(name_protein){
  filter(table_peptyd, proteins == name_protein)
}

search_proteins("HPRR1060049") # przykład działania funkcji 


#=========================(II sposób na tworzenie tabeli peptydów) ==============================
# sztuczne stworzenie kolumny zawierania peptydów w białkach
table_protein_2 <- table_protein
table_protein_2$include <- rep(1, dim(table_protein)[1]) 

# tworzenie data.frame zawierania, gdzie nazwami kolumn są nazwy protein.id, 
# od górnie sortuje po nazwie sequence i protein.id
data_protein_2 <- spread(table_protein_2, protein.id, include, fill = 0)
data_protein_2[1:5, 1:5]
class(data_protein_2)
dim(data_protein_2)

# usuwamy pierwszą kolumnę z nazwami sequence oraz usuwamy nazwy kolumn, 
# aby otrzymać macierz zer i jedynek jako liczby
matrix_protein_2 <- data_protein_2[,2:dim(data_protein_2)[2]]
head(matrix_protein_2)
colnames(matrix_protein_2) <- NULL
matrix_protein_2 <- as.matrix(matrix_protein_2) # dopiero tu jest macierzą
class(matrix_protein_2)
matrix_protein_2[1:5,1:5]

# macierz wspólnych peptydów
matrix_peptid_2 <- t(matrix_protein_2) %*% matrix_protein_2
matrix_peptid_2[1:5,1:5]

# tabela peptydów
table_peptid_2 <- data.frame(proteins = colnames(data_protein_2)[2:dim(data_protein_2)[2]], 
                             qty_peptids = diag(matrix_peptid_2))
head(table_peptid_2)
#===============================================================================

