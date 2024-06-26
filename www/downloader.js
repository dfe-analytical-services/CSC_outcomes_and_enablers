// This cell function is used to replace the negative numbers with suppression letters
// Allows the downloaded data to keep the numerical and alphabetical ordering of the data.
function cellFunction(value) {
  if (value == -100) {
    return 'c';
  } else if (value == -200) {
    return 'k';
  } else if (value == -250) {
    return 'u';
  } else if (value == -300) {
    return 'x';
  } else if (value == -400) {
    return 'z';
  } else {
    return value;
  }
}

function customDownloadDataCSV(id, filename) {
  console.log("customDownloadDataCSV called with id:", id, "and filename:", filename);

  // Retrieve CSV data using Reactable.getDataCSV
  var csvData = Reactable.getDataCSV(id, { headers: true, sep: ',', dec: '.' });
  
  if (!csvData) {
    console.error("Failed to retrieve CSV data for table id:", id);
    return;
  }
  
  // Split CSV data into rows
  var rows = csvData.split('\n');
  
  // Apply cellFunction to each cell in the CSV
  var processedRows = rows.map(function(row, rowIndex) {
    return row.split(',').map(function(cell, cellIndex) {
      return rowIndex === 0 ? cell : cellFunction(cell);
    }).join(',');
  });

  var csvString = processedRows.join('\n');
  console.log("CSV string after processing:", csvString);

  // Create and trigger download
  var blob = new Blob([csvString], { type: 'text/csv;charset=utf-8;' });
  var link = document.createElement('a');
  if (link.download !== undefined) {
    var url = URL.createObjectURL(blob);
    link.setAttribute('href', url);
    link.setAttribute('download', filename);
    link.style.visibility = 'hidden';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    console.log("CSV download triggered for", filename);
  }
}


