#ifndef ADATAPAGEMODEL_H
#define ADATAPAGEMODEL_H

#include <QtCore/QAbstractTableModel>
#include <QtCore/QMap>

typedef QMap<int, QString> ADataPageHeader;
typedef QMap<int, double> ADataPageRow;
typedef QMap<int, ADataPageRow> ADataPageTable;

class ADataPageModel : public QAbstractTableModel
{
    Q_OBJECT

public:

    /*!
      \brief Class constructor.
      */
    ADataPageModel(QObject *parent = 0, int rowCount = 100, int columnCount = 26);

    /*!
      \brief Returns the no. of rows which will be displayed in the view.
      */
    int rowCount(const QModelIndex &parent = QModelIndex()) const;

    /*!
      \brief Returns the no. of data rows. Does not include the "var" row.
      */
    int dataRowCount() const;

    /*!
      \brief Returns the no. of columns which will be displayed in the view.
      */
    int columnCount(const QModelIndex &parent = QModelIndex()) const;

    /*!
      \brief Returns the data for a given row and column.
      */
    QVariant data(const QModelIndex &index, int role) const;

    /*!
      \brief Sets the row count.
      */
    void setRowCount(int rowCount);

    /*!
      \brief Sets the column count.
      */
    void setColumnCount(int columnCount);

    /*!
      \brief Returns the header label.
      */
    QVariant headerData(int section, Qt::Orientation orientation, int role) const;

    /*!
      \brief Sets the data of specified row and column.
      */
    bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole);

    /*!
      \brief Returns the item flags.
      */
    Qt::ItemFlags flags(const QModelIndex &index) const;

    /*!
      \brief Returns the list of active columns (not empty var column).
      */
    const ADataPageHeader& getActiveColumns() const;

    /*!
      \brief Returns the list of active rows.
      */
    const ADataPageTable& getActiveRows() const;

    /*!
      \brief Clears all the data.
      */
    void clear();

    /*!
      \brief Save data to specified file.
      */
    bool saveToFile(const QString &fileName);

    /*!
      \brief Load data from specified file.
      */
    bool loadFromFile(const QString &fileName);

    /*!
      \brief Converts the data to an AIS object vector string constant.
      */
    QString toStringConstantObjVector(const QList<int> indexes);

    /*!
      \brief Converts the data to an AIS number vector string constant.
      */
    QString toStringConstantNumVector(int index);

    /*!
      \brief Returns true if model is dirty.
      */
    bool isDirty();

    /*!
      \brief Sets the dirty flag.
      */
    void setDirty(bool dirty = true);

    /*!
      \brief Delete specified items.
      */
    void deleteItems(const QModelIndexList &indexList);

private:

    int cRowCount;                  // no. of rows (displayed)
    int cColCount;                  // no. of columns (displayed)
    ADataPageTable cData;           // data
    ADataPageHeader cDataHeader;    // header information
    bool cIsDirty;                  // dirty flag
};

#endif // ADATAPAGEMODEL_H
