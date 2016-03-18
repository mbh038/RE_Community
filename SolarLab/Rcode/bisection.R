
true<-runif(1)*100
max=100
min=0
guess<-(max-min)/2
count=0
print(paste("True: ",true," Guess: ",guess," iterations: ",count))
while (abs(guess-true)>1){
    count=count+1
    diff=guess-true
    if (diff>1){
        max=guess
        guess<-min+(max-min)/2
    }
    if(diff<1){
       min=guess
       guess<-min+(max-min)/2
    }
    print(paste("True: ",true," Guess: ",guess," iterations: ",count))
    #if (count>10) break
}



