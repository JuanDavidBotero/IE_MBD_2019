# coding=utf-8
from pyspark.sql import SparkSession

spark = SparkSession \
    .builder \
    .appName("Individual_Assignment_SQL") \
    .getOrCreate()

# DataFrame creation from JSON file
df = spark.read.json("data/purchases.json")

# Basic Operations
df.printSchema()
df.describe().show()
df.show(1)

# Spark SQL
df.createOrReplaceTempView("purchases")

print ""
print "Answers: Juan David Botero"
print ""

# 1. Top 10 most purchased products
print "1. Top 10 most purchased products:"
top_10 = spark.sql(
    "SELECT product_id, item_type, count(product_id) as count"
    " FROM purchases"
    " group by product_id, item_type"
    " order by count DESC"
    " limit 10"
)
top_10.show()

# 2. Purchase percentage of each product type (item_type)
print "2. Purchase percentage of each product type:"
percen_type = spark.sql(
    "select item_type, count(item_type) as count,"
    " (count(item_type) / (select count(*) from purchases)) * 100 as percentage_of_total"
    " from purchases"
    " group by item_type"
    " order by percentage_of_total DESC"
)
percen_type.show()

# 3. Shop that has sold more products
print "3. Shop that has sold more products:"
top_shop_product = spark.sql(
    "select shop_id, count(product_id) as num_products "
    " from purchases"
    " group by shop_id"
    " order by num_products DESC"
    " limit 1"
)
top_shop_product.show()

# 4. Shop that has billed more money
print "4. Shop that has billed more money:"
top_shop = spark.sql(
    "select shop_id, sum(price) as sales "
    " from purchases"
    " group by shop_id"
    " order by sales DESC"
    " limit 1"
)
top_shop.show()

# 5. Divide world into 5 geographical areas based in longitude (location.lon)
# and add a column with geographical area name, for example “area1”, “area2”, …(longitude values are not important)

print "Total range of values 179.8282 + 179.937 = 359.7652 / 5 = 71.95304 (size of each bucket)"
print "area1: less than -107.87516"
print "area2: -107.87517 : -35.92212"
print "area3: -35.92213 : 36.03092"
print "area4: 36.03093 : 107.98396"
print "area5: more than 107.98397"

area = spark.sql(
    "SELECT *,"
    " CASE WHEN location.lon < -107.87516 THEN 'area1'"
    " WHEN location.lon BETWEEN -107.87517 AND -35.92212 THEN 'area2'"
    " WHEN location.lon BETWEEN -35.92213 AND 36.03092 THEN 'area3'"
    " WHEN location.lon BETWEEN 36.03093 AND 107.98396 THEN 'area4'"
    " ELSE 'area5'"
    " END AS areas"
    " FROM purchases"
)

area.createOrReplaceTempView("df_areas")

print ""
print "5. Divide world into 5 geographical areas based in longitude (location.lon): "
area.show()

# a. In which area is PayPal most used
print "Areas with the most payments done by paypal:"
paypal = spark.sql(
    "select areas, payment_type, count(areas) as total"
    " from df_areas"
    " where payment_type = 'paypal'"
    " group by areas, payment_type"
    " order by total DESC"
    " limit 1"
)
paypal.show()

# b. Top 3 most purchased products in each area
d_top3_by_area = spark.sql (
    " select * from "
    " (select areas, product_id, item_type, count(product_id) as num_products"
    " from df_areas"
    " where areas = 'area1'"
    " group by areas, product_id, item_type"
    " order by num_products DESC"
    " LIMIT 3)"
    " UNION ALL"
    " (select areas, product_id, item_type, count(product_id) as num_products"
    " from df_areas"
    " where areas = 'area2'"
    " group by areas, product_id, item_type"
    " order by num_products DESC"
    " LIMIT 3)"
    " UNION ALL"
    " (select areas, product_id, item_type, count(product_id) as num_products"
    " from df_areas"
    " where areas = 'area3'"
    " group by areas, product_id, item_type"
    " order by num_products DESC"
    " LIMIT 3)"
    " UNION ALL"
    " (select areas, product_id, item_type, count(product_id) as num_products"
    " from df_areas"
    " where areas = 'area4'"
    " group by areas, product_id, item_type"
    " order by num_products DESC"
    " LIMIT 3)"
    " UNION ALL"
    " (select areas, product_id, item_type, count(product_id) as num_products"
    " from df_areas"
    " where areas = 'area5'"
    " group by areas, product_id, item_type"
    " order by num_products DESC"
    " LIMIT 3)"
)

print'Top 3 most purchased products in each area: '
d_top3_by_area.show()


# c. Area that has billed less money
print "Least sales by areas:"
least_money = spark.sql(
    "select areas, sum(price) as total_sales"
    " from df_areas"
    " group by areas"
    " order by total_sales"
)
least_money.show()

# 6. Products that do not have enough stock for purchases made
print "Read the stock.csv file:"
print ""

stock = spark.read.csv("data/stock.csv", header=True)
stock.createOrReplaceTempView("stock")

print "6. Products that do not have enough stock for purchases made:"
no_stock = spark.sql(
    "select b.product_id, count(b.product_id) as purchased, a.quantity"
    " from stock a inner join df_areas b on a.product_id = b.product_id" 
    " group by b.product_id, a.quantity"
    " having a.quantity < purchased"
)
no_stock.show()