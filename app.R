library(dash)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(purrr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
data_df <- read.csv("data/processed_data.csv")
price_subset <- data.frame(
    title=c("Total Monthly Cost",
            "Basic Groceries",
            "Childcare", 
            "Entertainment",
            "Fitness", 
            "Monthly Rent",
            "Public Transport",
            "Shopping",
            "Utilities") , 
    feature=c("all",
              "grocery_for_one_person",
              "childcare_for_one_child",
              "entertainment",
              "fitness",
              "rent_for_one_person",
              "transportation_public",
              "shopping",
              "utility_bills"))


app$layout(
    dbcContainer(
        list(
            dccGraph(id='plot-area'),
            dccDropdown(
                id='city_name',
                options = data_df$city %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                value='New York',
                multi=T),
            dccDropdown(
                id='cost_subset',
                options = price_subset$feature %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                value='all')
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('city_name', 'value'),
         input('cost_subset', 'value')),
    function(city_name,cost_subset) {
        subset <- data_df %>% 
            filter(city %in% city_name)
        y_title <- price_subset$title[price_subset$feature == cost_subset]
        
        p <- ggplot(subset, aes(x = city,
                                y = !!sym(cost_subset),
                                fill = city
        )) +
            labs(x = 'City', y = y_title) +
            geom_col() 
        ggplotly(p)
    }
)

app$run_server(host = '0.0.0.0')
