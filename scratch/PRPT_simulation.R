  numiters = 1000 #number of iterations
  numtrials = 150 #number of trials

  Random_earnings = matrix(nrow = numiters, ncol = 1)
  Random_correctness = matrix(nrow = numiters, ncol = 1)
  Random_correctness_run = matrix(nrow = numiters, ncol = numtrials)


  for (k in 1:numiters) {

  nEpisode = 4
  reward_episode = matrix(nrow = 4, ncol = numtrials)
  choice = matrix(nrow = numtrials, ncol = 2)
  decision = matrix(nrow = 1, ncol = numtrials)
  percent_correct = matrix(nrow = 1, ncol = numtrials)

  Reward_vector = matrix(nrow = 6, ncol = 100)
  Reward_structure = matrix(nrow = 6, ncol = 2)
  Reward_vector[1,] <- c(rep(0,75),rep(1,25))
  Reward_vector[2,] <- c(rep(0,50),rep(1,50))
  Reward_vector[3,] <- c(rep(0,25),rep(1,75))
  Reward_vector[4,] <- c(rep(0,75),rep(1,25))
  Reward_vector[5,] <- c(rep(0,50),rep(1,50))
  Reward_vector[6,] <- c(rep(0,25),rep(1,75))
  Reward_structure[1,] <- c(1,2.5)
  Reward_structure[2,] <- c(1,1.5)
  Reward_structure[3,] <- c(1,0.5)
  Reward_structure[4,] <- c(-1,-1.25)
  Reward_structure[5,] <- c(-1,-0.75)
  Reward_structure[6,] <- c(-1,-0.25)

  Choice_vector = c(rep(0,25), rep(1,25))
  Choice_vector_shuffled = sample(Choice_vector,50)


  for (i in 1:numtrials) {

      # Phase One choices
      if (i < 25 || i == 25) {
            choice[i,] <- sample(c(1,2,3),2)}

      # Phase Two choices
      if (i > 25 && i <=75) {
            if (Choice_vector_shuffled[i-25] == 0)
                    choice[i,] <- sample(c(1,2,3),2)
            if (Choice_vector_shuffled[i-25] == 1)
                    choice[i,] <- sample(c(4,5,6),2)}

      # Phase 3 choices
      if (i > 75) {
              choice[i,] <- sample(c(1,2,3,4,5,6),2)}
  }


  for (i in 1:numtrials) {

      # Create variable ('Decision_vector') with 50/50 chance of choosing choice
      # 1 or choice 2, shuffle 'Decision_vector' x1000, and make decision
      # according to first element of shuffled 'Decision_vector'
      Decision_vector <- c(rep(choice[i,1],50), rep(choice[i,2],50))
      Decision_vector_shuffled <- sample(Decision_vector,100)
      decision[1,i] <- Decision_vector_shuffled[1]


      # Determine whether reward is obtained or not. If the first element of
      # 'Reward_vector_shuffled'=1, then reward is obtained. The reward
      # magnitude depends on the trial # (different reward magnitudes by task
      # Phase).
      Reward_vector_trial <- Reward_vector[decision[1,i],]
      Reward_vector_shuffled <- sample(Reward_vector_trial,100)
      if (i < 76) {
          if (Reward_vector_shuffled[1] == 1) {
              reward_episode[3,i] <- Reward_structure[decision[1,i],1]}
          else {
              reward_episode[3,i] <- 0 }
      }

      if (i > 75) {
          if (Reward_vector_shuffled[1] == 1) {
              reward_episode[3,i] <- Reward_structure[decision[1,i],2]}
          else {
              reward_episode[3,i] = 0 }
      }

      # Determine -- for Phases 1/2 -- whether the choice made by the agent was
      # the "Correct" choice, i.e. the agent chose the option with the highest
      # expected value.
      if (i < 76) {
          if (decision[1,i] < 4) {
              if (((choice[i,1] == 1) && (choice[i,2] == 2)) || ((choice[i,1] == 2) && (choice[i,2] == 1))) {
                  if (decision[1,i] == 1) { percent_correct[1,i] <- 0 }
                  else { percent_correct[1,i] <- 1 } }

              else if (((choice[i,1] == 1) && (choice[i,2] == 3)) || ((choice[i,1] == 3) && (choice[i,2] == 1))) {
                  if (decision[1,i] == 1) { percent_correct[1,i] <- 0 }
                  else { percent_correct[1,i] <- 1 } }

              else if (((choice[i,1] == 2) && (choice[i,2] == 3)) || ((choice[i,1] == 3) && (choice[i,2] == 2))) {
                  if (decision[1,i] == 2) { percent_correct[1,i] <- 0 }
                  else { percent_correct[1,i] <- 1 } } }

          else if (decision[1,i] > 3) {
                if (((choice[i,1] == 4) && (choice[i,2] == 5)) || ((choice[i,1] == 5) && (choice[i,2] == 4))) {
                    if (decision[1,i] == 4) {
                        percent_correct[1,i] <- 1 }
                    else {
                        percent_correct[1,i] <- 0 } }

                else if (((choice[i,1] == 4) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 4))) {
                    if (decision[1,i] == 4) {
                        percent_correct[1,i] <- 1 }
                    else {
                        percent_correct[1,i] <- 0 } }

                else if (((choice[i,1] == 5) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 5))) {
                    if (decision[1,i] == 5) {
                        percent_correct[1,i] <- 1 }
                    else {
                        percent_correct[1,i] <- 0 } } }
        }

      # Determine -- for Phases 3 -- whether the choice made by the agent was
      # the "Correct" choice, i.e. the agent chose the option with the highest
      # expected value.
      if (i > 75) {
          if (((choice[i,1] == 1) && (choice[i,2] == 2)) || ((choice[i,1] == 2) && (choice[i,2] == 1))) {
              if (decision[1,i] == 2) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 1) && (choice[i,2] == 3)) || ((choice[i,1] == 3) && (choice[i,2] == 1))) {
              if (decision[1,i] == 1) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 1) && (choice[i,2] == 4)) || ((choice[i,1] == 4) && (choice[i,2] == 1))) {
              if (decision[1,i] == 1) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 1) && (choice[i,2] == 5)) || ((choice[i,1] == 5) && (choice[i,2] == 1))) {
              if (decision[1,i] == 1) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 1) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 1))) {
              if (decision[1,i] == 1) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 2) && (choice[i,2] == 3)) || ((choice[i,1] == 3) && (choice[i,2] == 2))) {
              if (decision[1,i] == 2) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 2) && (choice[i,2] == 4)) || ((choice[i,1] == 4) && (choice[i,2] == 2))) {
              if (decision[1,i] == 2) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 2) && (choice[i,2] == 5)) || ((choice[i,1] == 5) && (choice[i,2] == 2))) {
              if (decision[1,i] == 2) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 2) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 2))) {
              if (decision[1,i] == 2) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 3) && (choice[i,2] == 4)) || ((choice[i,1] == 4) && (choice[i,2] == 3))) {
              if (decision[1,i] == 3) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 3) && (choice[i,2] == 5)) || ((choice[i,1] == 5) && (choice[i,2] == 3))) {
              if (decision[1,i] == 3) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 3) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 3))) {
              if (decision[1,i] == 3) {
                  percent_correct[1,i] = 1 }
              else {
                    percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 4) && (choice[i,2] == 5)) || ((choice[i,1] == 5) && (choice[i,2] == 4))) {
              if (decision[1,i] == 4) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 4) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 4))) {
              if (decision[1,i] == 6) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

          else if (((choice[i,1] == 5) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 5))) {
              if (decision[1,i] == 6) {
                  percent_correct[1,i] = 1 }
              else {
                  percent_correct[1,i] = 0 } }

      }
  }


  # Calculate and store the total earnings and the overall percent correct for
  # each simulation
  Random_earnings[k] <- sum(reward_episode[3,])
  Random_correctness[k] <- (sum(percent_correct)/150)
  Random_correctness_run[k,] <- percent_correct

  print(k)

  }

  hist(Random_earnings)
  hist(Random_correctness)
