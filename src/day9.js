const fs = require('fs')
const log = console.log

const input = fs.readFileSync('./day9input.txt', { encoding: 'utf-8' })
const data = input.split("\n").map((row, rowId) => row.split("").map((c, cId) => [`${rowId},${cId}`, Number(c)])).flat()

const dict = Object.fromEntries(data)

const findLowestPoints = d => {
  let res = []
  let [maxRow, maxCol] = data[data.length - 1][0].split(",").map(Number)
  let row = 0
  while (row <= maxRow) {
    let col = 0
    while (col <= maxCol) {
      let num = d[`${row},${col}`]
      let neighbors = [
        `${row},${col + 1}`,
        `${row},${col - 1}`,
        `${row + 1},${col}`,
        `${row - 1},${col}`,
      ].map(k => d[k]).filter(d => d !== undefined)
      if (neighbors.every(item => item > num)) res.push({ [`${row},${col}`]: num })
      col += 1
    }
    row += 1
  }
  return res.flatMap(Object.keys)
}


const getNeighbors = (point) => {
  let [row, col] = point.split(",").map(Number)
  return [
    `${row},${col + 1}`,
    `${row},${col - 1}`,
    `${row + 1},${col}`,
    `${row - 1},${col}`,
  ]
}

const getBasins = (point, dict, basins) => {
  basins.add(point)
  let neighbors = getNeighbors(point).filter(k => dict[k] !== 9 && dict[k] > dict[point])
  neighbors.forEach(n => {
    basins.add(n)
    getBasins(n, dict, basins)
  })
  return basins
}

const main = () => {
  const lowestPoints = findLowestPoints(dict)
  const basins = lowestPoints.map(p => getBasins(p, dict, new Set())).sort((a, b) => b.size > a.size ? 1 : -1)
  const [a, b, c, ...rest] = basins
  log(a.size * b.size * c.size)
}

main()