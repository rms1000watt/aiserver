#include "adatapagemodel.h"
#include <QtGui/QFont>
#include <QtCore/QFileInfo>
#include <QtCore/QStringList>
#include <QtCore/QTextStream>

ADataPageModel::ADataPageModel(QObject *parent,
                               int rowCount,
                               int columnCount)
                                   : QAbstractTableModel(parent),
                                   cRowCount(rowCount),
                                   cColCount(columnCount)
{
    cIsDirty = true;
}

int ADataPageModel::rowCount(const QModelIndex &parent) const
{
    Q_UNUSED(parent);
    return (cRowCount + 1);
}

int ADataPageModel::dataRowCount() const
{
    return (cRowCount);
}

int ADataPageModel::columnCount(const QModelIndex &parent) const
{
    Q_UNUSED(parent);
    return cColCount;
}

void ADataPageModel::setRowCount(int rowCount)
{
    cRowCount = rowCount;
    reset();
}

void ADataPageModel::setColumnCount(int columnCount)
{
    cColCount = columnCount;
    reset();
}

QVariant ADataPageModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();

    if (role == Qt::DisplayRole || role == Qt::EditRole)
    {
        int r = index.row();
        int c = index.column();

        if ((r == 0) && cDataHeader.contains(c))
        {
            return cDataHeader[c];
        }
        else if (cData.contains(r) && cData[r].contains(c))
        {
            if (role == Qt::EditRole)
                return QString("%1").arg(cData[index.row()][index.column()]);
            else
                return QString::number(cData[index.row()][index.column()], 'f', 3);
        }

        return QString("");
    }
    else if (role == Qt::TextAlignmentRole)
    {
        int r = index.row();
        if (r == 0)
        {
            return Qt::AlignHCenter;
        }

        return Qt::AlignRight;
    }
    else if (role == Qt::FontRole)
    {
        int r = index.row();
        if (r == 0)
        {
            QFont font;
            font.setItalic(true);
            font.setStyleHint(QFont::TypeWriter);
            return font;
        }
    }

    return QVariant();
}

QVariant ADataPageModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole)
    {
        if (orientation == Qt::Horizontal)
        {
            return QString("%1").arg(QChar('A' + section));
        }
        else if (orientation == Qt::Vertical)
        {
            if (section == 0)
            {
                return QString("var");
            }
            else
            {
                return QString("%1").arg(section);
            }
        }
    }
    else if (role == Qt::TextAlignmentRole)
    {
        if (orientation == Qt::Vertical)
        {
            return Qt::AlignHCenter;
        }
    }

    return QVariant();
}

bool ADataPageModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
    if (!index.isValid())
        return false;

    if (role == Qt::EditRole)
    {
        int r = index.row();
        int c = index.column();

        if (r == 0)
        {
            cDataHeader[c] = value.toString().trimmed();
        }
        else
        {
            if (value == "")
            {
                if (cData[r].contains(c))
                {
                    // remove column
                    cData[r].remove(c);
                    // remove row if no more columns
                    if (cData.value(r).isEmpty())
                    {
                        cData.remove(r);
                    }
                }
                else
                    return false;
            }
            else
            {
                cData[r][c] = value.toDouble();
            }
        }

        emit dataChanged(index, index);
        setDirty();

        return true;
    }

    return false;
}

Qt::ItemFlags ADataPageModel::flags(const QModelIndex &index) const
{
    return (QAbstractTableModel::flags(index) | Qt::ItemIsEditable);
}

const ADataPageHeader& ADataPageModel::getActiveColumns() const
{
    return cDataHeader;
}

const ADataPageTable& ADataPageModel::getActiveRows() const
{
    return cData;
}

void ADataPageModel::clear()
{
    beginResetModel();

    cDataHeader.clear();
    cData.clear();

    endResetModel();

    setDirty();
}

bool ADataPageModel::loadFromFile(const QString &fileName)
{
    QFileInfo fileInfo (fileName);
    QChar sep;

    if (fileInfo.suffix().toLower() == "csv")
    {
        // entries are separated by commas
        sep = ',';
    }
    else if (fileInfo.suffix().toLower() == "txt")
    {
        // entries are separated by tabs
        sep = '\t';
    }
    else
    {
        // extension not supported
        return false;
    }

    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly))
    {
        // cannot open file for writing
        return false;
    }

    beginResetModel();

    cDataHeader.clear();
    cData.clear();

    QString data = file.readAll();
    QStringList rows = data.split('\n', QString::SkipEmptyParts);

    QListIterator<QString> row(rows);
    int r = 0;
    int c = 0;
    if (row.hasNext())
    {
        QStringList cols = row.next().trimmed().split(sep);
        QListIterator<QString> col(cols);
        c = 0;
        while (col.hasNext())
        {
            cDataHeader[c] = col.next();
            c++;
        }
    }

    if (c > columnCount())
    {
        setColumnCount(c);
    }

    while (row.hasNext())
    {
        r++;
        QStringList cols = row.next().trimmed().split(sep);
        QListIterator<QString> col(cols);
        c = 0;
        while (col.hasNext())
        {
            cData[r][c] = col.next().toDouble();
            c++;
        }
    }

    if (r > dataRowCount())
    {
        setRowCount(r);
    }

    endResetModel();

    setDirty();

    return true;
}

bool ADataPageModel::saveToFile(const QString &fileName)
{
    QFileInfo fileInfo (fileName);
    QChar sep;

    if (fileInfo.suffix().toLower() == "csv")
    {
        // entries are separated by commas
        sep = ',';
    }
    else if (fileInfo.suffix().toLower() == "txt")
    {
        // entries are separated by tabs
        sep = '\t';
    }
    else
    {
        // extension not supported
        return false;
    }

    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly))
    {
        // cannot open file for writing
        return false;
    }

    QTextStream out(&file);

    QList<int> keys = cDataHeader.keys();
    if (keys.count() > 0)
    {
        for (int i = 0; i < keys.back(); ++i)
        {
            if (cDataHeader.contains(i))
            {
                out << cDataHeader.value(i);
            }

            out << sep;
        }

        // write final column header
        out << cDataHeader.value(keys.back()) << endl;
    }

    keys = cData.keys();
    if (keys.count() > 0)
    {
        for (int i = 1; i <= keys.back(); ++i)
        {
            if (cData.contains(i))
            {
                QList<int> colKeys = cData[i].keys();
                if (colKeys.count() > 0)
                {
                    for (int j = 0; j < colKeys.back(); ++j)
                    {
                        if (cData[i].contains(j))
                        {
                            out << cData[i][j];
                        }

                        out << sep;
                    }

                    // write final column data
                    out << cData[i][colKeys.back()];
                }
            }

            out << endl;
        }
    }

    return true;
}

QString ADataPageModel::toStringConstantNumVector(int index)
{
    QString numVector("#(num|");
    QMapIterator<int, ADataPageRow> row(cData);

    while (row.hasNext())
    {
        numVector.append(QString(" %1").arg(row.next().value().value(index)));
    }
    numVector.append(")");

    return numVector;
}

QString ADataPageModel::toStringConstantObjVector(const QList<int> indexes)
{
    QString objVector("#(obj|");
    QMapIterator<int, ADataPageRow> row(cData);
    QListIterator<int> var(indexes);

    while (row.hasNext())
    {
        // each row is a num vector
        objVector.append(" #(num|");
        const ADataPageRow &cols = row.next().value();

        var.toFront();
        while (var.hasNext())
        {
            objVector.append(QString(" %1").arg(cols.value(var.next())));
        }

        objVector.append(")");
    }

    objVector.append(")");

    return objVector;
}

bool ADataPageModel::isDirty()
{
    return cIsDirty;
}

void ADataPageModel::setDirty(bool dirty)
{
    cIsDirty = dirty;
}

void ADataPageModel::deleteItems(const QModelIndexList &indexList)
{
    QListIterator<QModelIndex> iter(indexList);
    while (iter.hasNext())
    {
        setData(iter.next(),"");
    }
}
