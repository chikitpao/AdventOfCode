// by CryZe92
// https://www.reddit.com/r/adventofcode/comments/5hoia9/2016_day_11_solutions/
// https://cryze.github.io/advent-of-code-2016/day-11/
            var config = {};
            // var codeXHR = new XMLHttpRequest();
            // codeXHR.open('GET', 'aoc.js', true);
            // codeXHR.onload = function() {
            //     var code = codeXHR.responseText;
            //     var blob = new Blob([code], {type: 'text/javascript'});
            //     codeXHR = null;
            //     var src = URL.createObjectURL(blob);
            //     var script = document.createElement('script');
            //     script.src = URL.createObjectURL(blob);
            //     script.onload = function() {
            //         URL.revokeObjectURL(script.src);
            //         var module = AoC(config);
            //         part1 = module.cwrap('part1', 'string', ['string']);
            //         part2 = module.cwrap('part2', 'string', ['string']);
            //     };
            //     document.body.appendChild(script);
            // }
            // codeXHR.send(null);

            let input = `The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.
The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
The third floor contains nothing relevant.
The fourth floor contains nothing relevant.`
            let elements = new Set();
            elements.add('elerium');
            elements.add('dilithium');
            // calculate(part1JS);
            calculate(part2JS);


            function calculate(code) {
                //var input = document.getElementById("input").value;
                //document.getElementById("output").value = "";
                var output = code(input);
                //document.getElementById("output").value = output;
                console.log(output);
            }

            function parse(str) {
                let floors = str.split('\n').map(line => {
                    return line.split('contains ')[1].split(/, and a |, a |and a |a /)
                        .map(x => x.replace(/(-compatible|\.)/g, '').trim())
                        .filter(x => !['', 'nothing relevant'].includes(x))
                        .map(x => x.split(' '))
                })

                floors.forEach(floor => floor.forEach(obj => elements.add(obj[0])));

                return [floors, 0]
            }

            function copy([floors, elevator]) {
                return [floors.map(floor => floor.slice(0)), elevator]
            }

            function isEndState([floors, ]) {
                return floors.slice(0, -1).every(floor => floor.length == 0)
            }

            function isValid([floors, elevator]) {
                return 0 <= elevator && elevator < floors.length && floors.every(floor => {
                    let generators = floor.filter(([, type]) => type == 'generator')

                    return generators.length == 0
                        || floor.every(([el, type]) => type == 'generator' || generators.some(([x, ]) => x == el))
                })
            }

            function* powerSet(array, n) {
                if (n == 0 || array.length == 0 || n > array.length) {
                    yield []
                    return
                }

                for (let i = 0; i < array.length - n + 1; i++) {
                    for (let rest of powerSet(array.slice(i + 1), n - 1)) {
                        rest.unshift(array[i])
                        yield rest
                    }
                }
            }

            function* listSteps(state) {
                let [floors, elevator] = state

                for (let n = 1; n <= 2; n++) {
                    for (let objects of powerSet(floors[elevator], n)) {
                        if (objects.length == 0) continue

                        if (objects.length == 2) {
                            let [[a, b], [c, d]] = objects
                            if (a != c && b != d) continue
                        }

                        for (let newElevator = elevator - 1; newElevator <= elevator + 1; newElevator += 2) {
                            if (newElevator < 0 || newElevator >= floors.length)
                                continue
                            if (newElevator < elevator && floors.slice(0, elevator).every(floor => floor.length == 0))
                                continue

                            let newState = copy(state)
                            let [newFloors, ] = newState
                            newState[1] = newElevator

                            for (let [el, type] of objects) {
                                let i = newFloors[elevator].findIndex(([x, y]) => x == el && y == type)
                                newFloors[newElevator].push(...newFloors[elevator].splice(i, 1))
                            }

                            newFloors[newElevator].sort()

                            if (isValid(newState)) yield newState
                        }
                    }
                }
            }

            function eqClass([floors, elevator]) {
                let n = floors.reduce((sum, floor) => sum + floor.length, 0) / 2
                let objects = [...Array(n)].map(_ => Array(2).fill(null))
                let names = []

                for (let i = 0; i < floors.length; i++) {
                    for (let [el, type] of floors[i]) {
                        let j = names.indexOf(el)

                        if (j < 0) {
                            names.push(el)
                            j = names.length - 1
                        }

                        objects[j][type == 'generator' ? 0 : 1] = i
                    }
                }

                return objects.sort().join(';') + ';' + elevator
            }

            // own function
            function getHash(state){
                let floor = state[1];
                let floors = state[0];
                let objects = new Array(16).fill(0);
                elements.forEach(element => {
                    let genFloor ;
                    let chipFloor;
                    floors.forEach((floor, floorIndex) => floor.forEach(obj => {
                        if((genFloor === undefined) && (obj[0] == element) && obj[1] == 'generator')
                            genFloor = floorIndex;
                        if((chipFloor === undefined) && (obj[0] == element) && obj[1] == 'microchip')
                            chipFloor = floorIndex;
                    }))
                    objects[genFloor * 4 + chipFloor]++;
                })
                let objectsHash = objects.map(v => v.toString());
                return floor.toString() + objectsHash.join('');
            }

            function bfs(state) {
                let queue = [state]
                let key = eqClass(state)
                let parents = {[key]: null}

                let getPath = function(end) {
                    let path = [end]
                    let key = eqClass(end)

                    while (parents[key] != null) {
                        path.push(parents[key])
                        key = eqClass(parents[key])
                    }

                    return path.reverse()
                }

                while (queue.length > 0) {
                    let current = queue.shift()
                    console.log(getHash(current));

                    if (isEndState(current)) return getPath(current)

                    for (let neighbor of listSteps(current)) {
                        let key = eqClass(neighbor)
                        if (key in parents) continue
                        parents[key] = current

                        queue.push(neighbor)
                    }
                }

                return null
            }


            function part1JS(input) {
                let state = parse(input)
                let path = bfs(state)
                return (path.length - 1);
            }

            function part2JS(input) {
                let state = parse(input)

                state[0][0].push(
                    ['elerium', 'generator'],
                    ['elerium', 'microchip'],
                    ['dilithium', 'generator'],
                    ['dilithium', 'microchip']
                )

                let path = bfs(state)
                return (path.length - 1);
            }