#Portfolio Optimization from lockdown uk, 16th march uk went into lockdown

library(tidyquant)
library(plotly)
library(timetk)
library(tidyr)
library(forcats)
#download price data
tickers<-c('CINE.L','CCL.L','NEX.L','BOO.L','SMWH.L')
Prices<-tq_get(tickers,from='2020-03-16',to='2020-09-03',get='stock.prices')
#calculate daily log returns 

log_returns<-Prices %>%group_by(symbol)%>% tq_transmute(select= adjusted,mutate_fun = periodReturn,
                                    period='daily',col_rename = 'Returns',type='log')
#data is tidy, use spread to convert it into wide format
#make into time series using xts
log_time_series<-log_returns %>%
  spread(symbol,value=Returns)%>%
tk_xts()  
#mean daily returns
mean_returns<-colMeans(log_time_series)
print(mean_returns)
# covariance matrix of annualized returns
Cov_xts_ret<-cov(log_time_series)*252
print(Cov_xts_ret)
eigen(Cov_xts_ret)
#the next step is to randomly simulate several weights to find the efficient frontier

NUmber_Of_Portfolios<-6000
#Matrix to store weights
Weights<-matrix(nrow=NUmber_Of_Portfolios,ncol=length(tickers))
#Store portfolio returns
Portfolio_Returns<-vector('numeric',length = NUmber_Of_Portfolios)
#Store Portfolio SD
Risk<-vector('numeric',length=NUmber_Of_Portfolios)
#Store Sharp Ratios
sharpe_ratio<-vector('numeric',length=NUmber_Of_Portfolios)


#6000 portfolios
for(i in seq_along(Portfolio_Returns)){
  port_weights<-runif(length(tickers))
  #ensures weights total to 1
  port_weights<-port_weights/sum(port_weights)
  #puts into weights matrix
Weights[i,]<-port_weights
port_Ret<-sum(port_weights*mean_returns)
port_Ret<-((port_Ret+1)^252)-1
#store portfolio returns
Portfolio_Returns[i]<-port_Ret
port_Risk<-sqrt(t(port_weights)%*%(Cov_xts_ret%*%port_weights))
Risk[i]<-port_Risk
#set a 0% risk free rate
SharpR<-port_Ret/port_Risk
sharpe_ratio[i]<-SharpR
}
Final_Portfolio<-tibble(Return=Portfolio_Returns,Risk=Risk,Sharpe_Rat=sharpe_ratio)
port_weights<-tk_tbl(port_weights)
colnames(port_weights)<-colnames(log_time_series)
Final_Portfolio<-tk_tbl(cbind(port_weights,Final_Portfolio))
head(Final_Portfolio)
MinVarPort<-Final_Portfolio[which.min(Final_Portfolio$Risk),]
MinVarPort
MaxSharpeRat<-Final_Portfolio[which.max(Final_Portfolio$Sharpe_Rat),]
#plot Min Var port weights
p<-MinVarPort%>% gather(BOO.L,key=Asset,value = Weights)%>%
  mutate(Asset=as.factor(Asset))%>% ggplot(aes(x=fct_reorder(Asset,Weights),y=Weights,fill=Asset))+
  geom_bar(stat='identity')+
  theme_minimal()+
  labs(x='Assets',y='Weights',title="Min Var Port Weights")+ scale_y_continuous(labels=scales::percent)

GGPLOTLY(p)
ggplotly(p)

Efficient_Frontier<-Final_Portfolio%>%
  ggplot(aes(x=Risk,y=Return,color=sharpe_ratio))+geom_point()+
  theme_classic()+scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::percent)+
  labs(x='Annualized Risk',y='Annualized Returns',title="Efficient Frontier")
ggplotly(Efficient_Frontier)
