# Name: Harsh Shah
# Final project

# All necessary libraries are downloaded
library(data.table)
library(maps)
library(RColorBrewer)
library(mapproj)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(treemap)
library(googleVis)
library(alluvial)
library(stringr)
library(leaflet)
library(sf)
library(here)
library(widgetframe)
library(scales)


# csv file is opened 
data_supplychain <- fread("C:/Users/harsh/Desktop/EM 622 Decision R/Final Exam/DataCoSupplyChainDataset.csv")
# column names are changed for better analysis
colnames(data_supplychain) <- c("Type","Days_for_shipping","Days_shipment_scheduled","Benefit_per_order","Sales_per_customer","Delivery_status","Late_delivery_risk","Category_Id","Category_name","Customer_city","Customer_Country","Customer_email","Cust_F","Cust_id","Cust_L","Cust_pass","Customer_segment","Customer_state","Customer_street","Customer_zipcode","Department_id","Department_name","Latitude","Longitude","Market","Order_city","Order_Country","Order_Customer_Id","order_date","Order_Id","Order_item_id","Order_Item_Discount", "Order_Item_Discount_Rate","Id_not","Product_price","Profit_ratio_order_item","Qty_order_item","Amount_Sales","Discounted_amount","Amount_profit_order","Order_region","Order_State","Order_status","ZIpcode_not","Id_2_not","id_3_not","Product_discription","Image","product_name","price_not","status_not","data_not","Shipping_mode")
# Data from USA is only considered
new_data_supplychain_frame <- subset(data_supplychain, Customer_Country != "Puerto Rico")
new_data_supplychain_frame$Customer_Country[new_data_supplychain_frame$Customer_Country == "EE. UU."] <- "US"
# Only complete order are considered
latest_frame <- subset(new_data_supplychain_frame, Order_status == "COMPLETE")
View(head(latest_frame))

# Kentucky State analysis
kentucky_frame <- subset(latest_frame, Customer_state == "KY")
ken_update <- kentucky_frame %>% count(Category_name, Market,Delivery_status, wt = Benefit_per_order)
colnames(ken_update)[4] <- "Profit"
View(ken_update)
# scatter plot for kentucky state
myplot_scatter_kentucky <- ggplot(data = ken_update, aes(x=Category_name,y=Profit)) + 
  geom_jitter(mapping = aes(colour=Delivery_status), alpha = 0.8)+
  facet_wrap(~Market, ncol = 5) + scale_y_continuous(labels = comma) +
  labs(title = "Profit Analysis for Kentucky and shipping mode", caption = "Created by Harsh Shah") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.title = element_text(face = "bold"),strip.background = element_rect(colour = "green", size = 2,fill = "yellow"),legend.text = element_text(color = "turquoise"))
myplot_scatter_kentucky

#Scatter Plot States
scatter_frame <- latest_frame %>% count(Customer_state, Shipping_mode,Delivery_status, wt = Benefit_per_order)
scatter_frame <- scatter_frame[-c(1,2), ]
View(scatter_frame)
colnames(scatter_frame)[4] <- "Profit"
# scatter plot for states
myplot_scatter <- ggplot(data = scatter_frame, aes(x=Customer_state,y=Profit)) + 
  geom_jitter(mapping = aes(colour=Shipping_mode), alpha = 0.8)+
  facet_wrap(~Delivery_status, ncol = 3) + scale_y_continuous(labels = comma) +
  labs(title = "Profit Analysis by states and shipping mode", caption = "Created by Harsh Shah") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.title = element_text(face = "bold"),strip.background = element_rect(colour = "green", size = 2,fill = "yellow"),legend.text = element_text(color = "turquoise"))
myplot_scatter

#Bar plot for oder distribution across states
Bar_frame <- latest_frame[!(latest_frame$Customer_state=="91732"|latest_frame$Customer_state=="95758"),]
ggplot(data = Bar_frame) + geom_bar(aes(x=Customer_state, y=..count.., fill=Department_name)) +
  labs(title = "Order Distribution with categories across states", caption = "Created by Harsh Shah")



# Interactive Geographical map
geo_dataframe <- latest_frame %>% count(Customer_state)
geo_dataframe <- geo_dataframe[-c(1,2), ]
View(geo_dataframe)
Geostates <- gvisGeoChart(geo_dataframe,"Customer_state","n",options = list(region="US", displayMode="regions",resolution="provinces",width=600,height=400))
plot(Geostates)


#Interactive Barchart
Bar_frame <- latest_frame %>% count(Order_region, wt = Amount_Sales)
colnames(Bar_frame) <- c("Order_region","Amount_sales")
Bar_frame_1 <- latest_frame %>% count(Order_region, wt = Amount_profit_order)
colnames(Bar_frame_1) <- c("Order_region","Amount_profit_order")
Bar_chart <- merge(Bar_frame,Bar_frame_1, by = 'Order_region')
View(Bar_chart)
Bar_graph <- gvisColumnChart(Bar_chart, options = list(legend='none',width=1000,height=1000)) 
plot(Bar_graph)  

#Merging the interactive plots
merge_frame <- gvisMerge(Geostates, Bar_graph, tableOptions = "cellspacing=\"20\" bgcolor=\"#AABBCC\"",horizontal = TRUE)
plot(merge_frame)


#TreeMap for risk distributed across states
treemap_dataframe <- latest_frame %>% count(Customer_state, wt = Late_delivery_risk)
New_tree <- treemap_dataframe[-c(1,2), ]
View(New_tree)
colnames(New_tree) <- c("Customer_state","Late_shipping_risk_days")
treemap(New_tree, index = c("Customer_state"), vSize = "Late_shipping_risk_days", vColor = "Late_shipping_risk_days", type = "value", palette = terrain.colors(10), title = "Highest shipping risk by region",fontsize.labels = c(15,8),border.col = "white")

#Scatter Plot for profit analysis
# random variables are choosen for netter analysis
random_data <- filter(latest_frame, latest_frame$Amount_profit_order > -1000) %>% sample_n(5000)
myplot <- ggplot(data = random_data, aes(x=Department_name,y=Amount_profit_order)) + 
  geom_jitter(mapping = aes(colour=Delivery_status, size=Qty_order_item), alpha = 0.8) + facet_wrap(~Customer_segment, ncol = 3) +
  labs(title = "Profit Analysis", caption = "Created by Harsh Shah") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.title = element_text(face = "bold"),strip.background = element_rect(colour = "green", size = 2,fill = "yellow"),legend.text = element_text(color = "turquoise")) 
myplot

myplot_2 <- ggplot(data = random_data, aes(x=Market,y=Amount_profit_order)) +
  facet_wrap(~Shipping_mode, ncol = 4) + 
  geom_jitter(mapping = aes(colour=Delivery_status), alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.title = element_text(face = "bold"),strip.background = element_rect(colour = "green", size = 2,fill = "yellow"),legend.text = element_text(color = "turquoise"))
myplot_2


#Heat Map for variables and regions
# calculation are calculated like sum, average
latest_frame <- latest_frame %>% group_by(Order_region) %>% mutate(Sum_profit = sum(Amount_profit_order))
latest_frame <- latest_frame %>% group_by(Order_region) %>% mutate(Sum_sales = sum(Amount_Sales))
latest_frame <- latest_frame %>% group_by(Order_region) %>% mutate(Sum_delivery_risk = sum(Late_delivery_risk))
latest_frame <- latest_frame %>% group_by(Order_region) %>% mutate(Average_delivery_time = mean(Days_for_shipping))
latest_frame <- latest_frame %>% group_by(Order_region) %>% mutate(Delivery_scheduled = mean(Days_shipment_scheduled))
View(latest_frame)
df <- distinct(latest_frame, Order_region, .keep_all = TRUE)
dataframe_table_heat <- df[c("Order_region","Sum_profit","Sum_sales","Sum_delivery_risk","Average_delivery_time","Delivery_scheduled")]
my_df <- as.data.frame(dataframe_table_heat)
tree_data <- latest_frame %>% count(Order_region)
Final_data_tree <- merge(my_df,tree_data, by = 'Order_region')
Tree_data_final <- as.data.frame(Final_data_tree)
row.names(Tree_data_final) <- Tree_data_final$Order_region
My_df_tree <- Tree_data_final[,2:7]
colnames(My_df_tree)[6] <- "Number_of_orders"
table_matrix <- data.matrix(My_df_tree)
heatmap_regions <- heatmap(table_matrix, Rowv = TRUE, Colv = NA, margins = c(15,5), col = brewer.pal(9,"Reds"),scale = "column",main = "Heatmap for profit and delivery variables according to order regions ", srt = 45)





