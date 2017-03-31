find_first_pos <- function(long_seq, short_seq) {
  n_long <- length(long_seq)
  n_short <- length(short_seq)
  for (i in 1:(n_long - n_short + 1)) {
    subseq <- long_seq[i:(i + n_short - 1)]
    if (isTRUE(all.equal(subseq, short_seq))) {
      return(i)
    }
  }
  return(Inf) # no pos found
}

long_seq <- c(1, 0, 1, 0, 1, 0, 1)
short_seq <- c(0, 1, 0)
find_first_pos(long_seq, short_seq)

# функция проводит n_iter экспериментов,
# чтобы понять, какая последовательность A или B выпадает раньше
# для проверки генерируются последовательности длины size = 30
# поэтому возможны ситуации ничьи, когда ни A, ни B не выпали
fight_AB <- function(short_seq_A, short_seq_B, n_iter = 2000, size = 30) {
  count_A <- 0
  count_B <- 0
  count_tie <- 0
  for (i in 1:n_iter) {
    long_seq <- sample(c(0, 1), size = size, replace = TRUE)
    pos_A <- find_first_pos(long_seq, short_seq_A)
    pos_B <- find_first_pos(long_seq, short_seq_B)
    if (pos_A < pos_B) {
      count_A <- count_A + 1
    } else if (pos_B <  pos_A) {
      count_B <- count_B + 1
    } else {
      count_tie <- count_tie + 1
    }
    
  }
  return(c(count_A, count_B, count_tie))
}

fight_AB(c(0, 1, 1), c(1, 0, 1), n_iter = 5000)
fight_AB(c(0, 0, 0), c(0, 1, 0), n_iter = 5000)

# создает все последовательности из 0 и 1 длины short_len
generate_all <- function(short_len) {
  all_combinations <- matrix(0, nrow = 2^short_len, short_len)
  for (i in 0:(2^short_len - 1)) {
    binary_vector <- rev(as.numeric(intToBits(i)))
    all_combinations[i + 1, ] <- tail(binary_vector, short_len)
  }
  return(all_combinations)
}

generate_all(3)

# соревнует между собой все последовательности длины short_len
# на выходе матрица p_{ij} = вероятность победы последовательности i над последовательностью j
fight_all <- function(short_len, n_iter = 5000) {
  n_seq <- 2^short_len
  fight_seq <- generate_all(short_len)
  fight_res <- matrix(0, nrow = n_seq, ncol = n_seq)
  
  for (i in 1:n_seq) {
    for (j in 1:n_seq) {
      message("Fight ", i, " vs ", j, "\n")
      if (i > j) {
        one_fight <- fight_AB(fight_seq[i, ], fight_seq[j, ], n_iter = n_iter)
        fight_res[i, j] <- one_fight[1] / n_iter
      }
    }
  }
  
  seq_names <- apply(fight_seq, 1, function(x) paste0(x, collapse = ""))
  colnames(fight_res) <- seq_names
  rownames(fight_res) <- seq_names
  
  # заполняем элементы выше главной диагонали
  fight_res_full <- fight_res - t(fight_res)
  fight_res_full <- fight_res_full + (fight_res_full < 0)
  return(fight_res_full)
}

res2 <- fight_all(2)
res2

res3 <- fight_all(3)
res3
