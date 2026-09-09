# n = # of data points per group
# a,b = slopes of Group A and B
n=50
a=1
b=2

# Generate Fake Data from groups A and B
x_A=1:n
x_B=1:n
y_A=a*x_A+rnorm(n)
y_B=10+b*x_B+rnorm(n)
df=data.frame(group=c(rep("A",n),rep("B",n)),
                     x=c(x_A,x_B),
                     y=c(y_A,y_B))

# Plot data
df |> 
  ggplot(aes(x=x,y=y,col=group))+
  geom_point(size=0.5)+
  geom_smooth(method=lm,se=FALSE)+
  scale_color_manual(values=c("#0072B2","#e35205"))

# Fit Model
fit_lm=lm(y~x*group,data=df)
summary(fit_lm)
