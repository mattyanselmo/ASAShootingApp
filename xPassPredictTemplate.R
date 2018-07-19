pred.data <- data.frame(home = 1, 
                        playerdiff = 0, 
                        x = 96,
                        y = 30,
                        angle = 0,
                        Position.model = "Field",
                        freekick = 0,
                        headpass = 0,
                        longball = 0,
                        throwin = 0,
                        throughball = 0,
                        cross = 0,
                        corner = 0,
                        first.pass = 0)

predict(success.gbm, pred.data, type = "response", n.trees = success.gbm$n.trees)
