"use strict"

///// AoC 2020 Day 21: Allergen Assessment 
///// Author: Chi-Kit Pao
/////
///// In firefox, you may need this setting to run a javascript program:
///// security.fileuri.strict_origin_policy = false
/////
///// Outputs:
///// Day 21
///// Question 1: Determine which ingredients cannot possibly contain any of the allergens in your list. How many times do any of those ingredients appear?
///// Answer: 2078
///// Question 2: What is your canonical dangerous ingredient list?
///// Answer: lmcqt,kcddk,npxrdnd,cfb,ldkt,fqpt,jtfmtpd,tsch
///// Execution time: 5 ms

const inputUrl = 'input.txt'
let isBrowser = true;
if (typeof window === 'undefined') {
    // Node.js
    isBrowser = false;
    getNodeJsInput(inputUrl).then(value => processInput(value, isBrowser));
} else {
    // Web API
    fetch(inputUrl).then(response => {
        response.text().then(value => processInput(value, isBrowser))
    });
}

async function getNodeJsInput(url) {
    const fs = await import('fs');
    return fs.readFileSync(url).toString();
}

function output(string, isBrowser) {
    console.log(string);
    if(isBrowser) {
        let bodys = document.getElementsByTagName('body');
        let p = document.createElement('p');
        p.textContent = string;
        bodys[0].appendChild(p);
    }
}

// JavaScript version is too old to have union / intersection / difference. 
// union / intersection / difference exist since JavaScript 2025.
function intersection(set1, set2) {
    if (set1 === null)
        return set2;
    let result = new Set();
    set2.forEach(el => {
        if (set1.has(el))
            result.add(el);
    });
    return result;
}
function difference(set1, set2) {
    let result = new Set();
    set1.forEach(el => {
        if (!set2.has(el))
            result.add(el);
    });
    return result;
}

function processInput(input, isBrowser){
    let start = Date.now();

    let ingredientSet = new Set();
    let allergenSet = new Set();
    let foods = new Array();

    const inputLines = input.split(/\r?\n/);
    for(let inputLine of inputLines) {
        if (inputLine.length == 0)
            continue;
        let inputLine2 = inputLine.substring(0, inputLine.length - 1);
        let inputParts = inputLine2.split(" (contains ");
        let ingredients = inputParts[0].split(" ");
        let allergens = inputParts[1].split(", ");

        ingredients.forEach(el => { ingredientSet.add(el) });
        allergens.forEach(el => { allergenSet.add(el) });
        foods.push(new Array(new Set(ingredients), new Set(allergens)));
    }

    let allergenIngredientMap = new Map();
    let ingredientAllergenMap = new Map();
    for (let allergen of allergenSet) {
        let intersectionSet = null;
        for (let food of foods) {
            if(food[1].has(allergen)) {
                if (intersectionSet === null) {
                    intersectionSet = food[0];
                } else {
                    intersectionSet = intersection(intersectionSet, food[0]);
                }
            }
        }
        allergenIngredientMap.set(allergen, intersectionSet)
    }
    while (true) {
        let newAllergen = null;
        let newIngredient = null;
        for (var [key, value] of allergenIngredientMap) {
            if (value.size == 1) {
                newIngredient = value.values().next().value;
                if(!ingredientAllergenMap.has(newIngredient)) {
                    newAllergen = key;
                    ingredientAllergenMap.set(newIngredient, newAllergen);
                    break;
                }
            }
        }
        if (newAllergen === null || newIngredient === null)
            break;
        allergenIngredientMap.forEach((value, key, _) => {
            if (key != newAllergen) {
                value.delete(newIngredient);
            }
        });
    }
    let allergenIngredients = new Set();
    for (const key of ingredientAllergenMap.keys()) {
         allergenIngredients.add(key);
    }

    output('Day 21', isBrowser);
    output('Question 1: Determine which ingredients cannot possibly contain any of the allergens in your list. How many times do any of those ingredients appear?', isBrowser);
    
    let answer1 = 0
    foods.forEach(f => { answer1 += difference(f[0], allergenIngredients).size });
    output('Answer: ' + answer1, isBrowser);

    output('Question 2: What is your canonical dangerous ingredient list?', isBrowser);

    let sortedAllergens = Array.from(allergenIngredientMap.keys()).sort();
    let mappedIngredients = sortedAllergens.map(a => allergenIngredientMap.get(a).values().next().value);
    output('Answer: ' + mappedIngredients.join(","), isBrowser);
    
    let end = Date.now();
    output(`Execution time: ${end - start} ms`, isBrowser);
}
