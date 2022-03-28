#Case: Simulation of Queues

S = 200


simulation = function(server, S){
  
  #存幾大指標 interval:客人間隔時間
  results = matrix(NA, nrow = S, ncol = 6, dimnames = list(NULL, c("total_cost", "idle_cost", "loss_cost", "wait_cost", "utilization", "interval")))
  
  
  for(s in 1:S){
    num_server = server
    num_idle_server = num_server #idle server???ƶq
    num_serving = 0 #???bserve???ƶq
    open_hour = 8
    open_minute = open_hour*60 #8小時變成480分鐘
    
    record_end_serve_time = double() # ????server?????A?ȫȤH???ɶ?
    
    num_loss_cust = 0 #num of loss customer 
    total_time_server_idle = 0 #the idle time of server 
    total_wait_time = 0 #the waiting time of server 
    
    
    queue = matrix(nrow = 0, ncol = 3,dimnames = list(NULL, c("cus_ID", "arr_time", "quit_time"))) 
    cust_list = matrix(nrow = 0, ncol = 4, dimnames = list(NULL, c("cus_ID", "arr_time", "quit_time", "description"))) #???????ѩҦ??ȤH
    ID_count = 1 #?_?l?ȤHID?s??
    
    
    ###simulate
    for (t in 1:open_minute) { # convert to minute
      #end serve, release server
      if(length(which(record_end_serve_time == t)) != 0){ 
        num_idle_server = num_idle_server + length(which(record_end_serve_time == t)) 
        num_serving = num_serving - length(which(record_end_serve_time == t))
        record_end_serve_time[which(record_end_serve_time == t)] = 0 
      }
      
      #check loss customer 
      num_leave = length(which(queue[, "quit_time"] == t)) #?h?֭ӫȤH?b?o?Ӯɶ?quit
      if(num_leave != 0){
        num_loss_cust = num_loss_cust + num_leave #????Loss?ȤH?ƶq
        queue = queue[-which(queue[, "quit_time"] == t), , drop = FALSE]#?q?????quit?ȤH
        cust_list[which(cust_list[, "quit_time"] == t), "description"] = paste("Loss at time:", t)#?????ȤH???A
      }
      
      #new customer coming 
      if(t <= (open_minute - 30)){ ###關店前30min不接客
        arrived_customer = rpois(1, 0.1) #6 customer per hour => 0.1 customer per minute
        if(arrived_customer != 0){ #?p?G???ȤH???F
          for(n in 1:arrived_customer){ #record each customer
            willing_wait_time = ceiling(rexp(1, 4) * 60) #0.25 per hour
            queue = rbind(queue, c(ID_count, t, t+willing_wait_time)) #???C?ӫȤH???i?ƶ?????
            cust_list = rbind(cust_list, c(ID_count, t, t+willing_wait_time, ""))
            ID_count = ID_count + 1
          }
        }
      }
      
      arrived_customer = 0 #reset arrived customer
      
      if(num_idle_server != 0){ # check ???S??idle server
        for(n in 1:num_idle_server){ #???C?@??idle server
          if(dim(queue)[1] == 0){ #if ?S?H?b?ƶ?
            total_time_server_idle = total_time_server_idle + 1 #?`idle?ɶ??[1
          }else{
            #get customer 
            id_ = queue[[1,"cus_ID"]]
            cust_wait_time = t - queue[[1, "arr_time"]] #calculate the wait time of the customer 
            total_wait_time = total_wait_time + cust_wait_time 
            queue = queue[-1,,drop = FALSE] #remove the customer from queue
            serve_time = round(rexp(1, 3) * 60) #simulate serve time and convert to minute 
            end_serve_time = t + serve_time #calculate the end time
            record_end_serve_time = c(record_end_serve_time, end_serve_time) #????serve?????ɶ?
            cust_list[which(cust_list[, "cus_ID"] == id_), "description"] = paste("Finish serve at:", end_serve_time)
            num_serving = num_serving + 1 #?[?W???bServe???ƶq
          }
        }
      }
      num_idle_server = num_server - num_serving #?p???ثeidle server?ƶq
    }#end t
    
    interval = mean(diff(as.numeric(cust_list[, "arr_time"]), lag = 1, differences = 1))
    #loss cost:30, idle cost(per hour):10 (per minute):10/60, wait cost(per hour):5 (per minute):5/60
    total_cost = num_loss_cust * 30 + total_time_server_idle * (1/6) + total_wait_time * (1/12)
    utilization = (num_server * 480 - total_time_server_idle) / (num_server * 480)
    results[s, ] = c(total_cost, total_time_server_idle * (1/6), num_loss_cust * 30, total_wait_time * (1/12), utilization,interval)
  }
  return(results)
}

##put the mean cost of different number of server
results = matrix(NA, nrow = 10, ncol = 6,
                 dimnames = list(NULL, c("avg_total_cost", "avg_idle_cost", "avg_loss_cost", "avg_wait_cost", "avg_utilization", "avg_interval")))
for(n in 1:10){ # 1??10?xserver
  sim_results = simulation(n, 200)
  results[n, ] = apply(sim_results, 2, mean) #calcalate mean cost of 200 simulation
}
results


plot(results[, "avg_total_cost"], type = "b", col = "blue",lwd=2, pch=15, main = "avg total cost",
     xlab = "number of server", ylab = "avg total cost")

plot(results[, "avg_idle_cost"], type = "b", col = "blue",lwd=2, pch=15, main = "avg idle cost",
     xlab = "number of server", ylab = "avg idle cost")

plot(results[, "avg_loss_cost"], type = "b", col = "blue",lwd=2, pch=15, main = "avg loss customer cost",
     xlab = "number of server", ylab = "avg loss customer cost")

plot(results[, "avg_wait_cost"], type = "b", col = "blue",lwd=2, pch=15, main = "avg wait cost",
     xlab = "number of server", ylab = "avg wait cost")

plot(results[, "avg_utilization"], type = "b", col = "blue",lwd=2, pch=15, main = "avg utilization",
     xlab = "number of server", ylab = "avg utilization")



###Verify Memoryless
Tsamples=rexp(S,1/10)
sum(Tsamples>5)/S
sum(Tsamples>15)/sum(Tsamples>10)


###Verify gamma distribution
S=10000
time.five=c()
for(s in 1:S){
  time.five[s]=sum(rexp(5,1/10))
}

min.x=round(min(time.five),2)
max.x=round(max(time.five),2)
x=seq(min.x, max.x, 0.01)
shape.est=5
scale.est=10
hist(time.five,breaks=50,freq=FALSE)
lines(x,dgamma(x,shape=shape.est,scale=scale.est),col='red',lwd=3)
lines(x,dgamma(x,shape=shape.est+1,scale=8),col='green',lwd=3)

x=seq(1,30,0.1)
plot(x,dexp(x,0.2),type='l',lwd=2)
lines(x,dgamma(x,shape=1,scale=1/0.2),col='red',lty=2)
lines(x,dgamma(x,shape=2,scale=1/0.2),col='blue')
lines(x,dgamma(x,shape=5,scale=1/0.2),col='green')

