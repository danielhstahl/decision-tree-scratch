entropy_fn=function(p){
    -p*log2(p)
}
entropy_base=function(p){
    #-sum(p*log2(p))
    sapply(p, function(p_inst){
        entropy_fn(p_inst)+entropy_fn(1-p_inst)
    })
}

compute_entropy=function(target, categorical_feature){
    p=aggregate(target, by=list(feature=categorical_feature), FUN=mean)
    individual_entropy=entropy_fn(p$x)
    largest_entropy=which.max(individual_entropy)
    max_value=p$feature[largest_entropy]
    list(entropy=entropy_base(p$x), max_value_by_entropy=max_value)
}
p=aggregate(target, by=list(feature=feature), FUN=mean)
compute_base_entropy_for_binary=function(target){
    p=mean(target)
    entropy_base(p)
}
compute_p_categorical_feature=function(categorical_feature){
    p=aggregate(categorical_feature, by=list(categorical_feature), FUN=length)/length(categorical_feature)
    p$x
}

information_gain=function(base_entropy, entropy, p_categorical_feature){
    base_entropy-sum(entropy*p_categorical_feature)
}

compute_tree=function(target, features, layer=1){
    print(paste("target size", length(target)))
    print(paste("layer", layer))
    num_features=ncol(features)
    print(num_features)
    if(length(target)>20 && layer<7){
        base_entropy=compute_base_entropy_for_binary(target)
        max_ig=0
        selected_feature=0
        best_value_of_feature=0
        index=1
        while(index<=num_features){
            feature=features[, index]
            entropy=compute_entropy(target, feature)
            p_feature=compute_p_categorical_feature(feature)
            ig=information_gain(base_entropy, entropy$entropy, p_feature)
            if(ig>max_ig){
                max_ig=ig
                selected_feature=index
                best_value_of_feature=entropy$max_value_by_entropy
            }
            index=index+1
        }
        best_feature=features[, selected_feature]
        print(paste("split at layer", layer))
        print(paste("split feature at index", selected_feature))
        print(paste("split feature on value", best_value_of_feature))
        print(paste("information gain of feature", max_ig))

        compute_tree(target[best_feature!=best_value_of_feature], as.matrix(features[best_feature!=best_value_of_feature, -selected_feature]), layer+1)
        compute_tree(target[best_feature==best_value_of_feature], as.matrix(features[best_feature==best_value_of_feature, -selected_feature]), layer+1)
        
    }
    else{
        print("done!")
    }
}

n=1000

feature1=rbinom(n, 2, 0.3)
feature2=rbinom(n, 2, 0.4)
feature3=rbinom(n, 2, 0.5)
target_p=exp(-2.5+feature1+feature2+feature3)
target_p=target_p/(1+target_p)
target=sapply(target_p, function(p){
    rbinom(1, 1, p)
})
data=cbind(feature1, feature2, feature3)
compute_tree(target, data)


